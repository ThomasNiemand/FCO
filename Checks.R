#### Check-Funktion ####

check_package <- function(pkg_dir,
                          mode = c("local", "server", "winbuilder", "docker"),
                          email = NULL,
                          server = NULL,
                          remote_dir = "~/rcheck",
                          win_versions = c("release", "devel"),
                          run_local_as_cran = TRUE,
                          docker_images = c("rocker/r-devel", "rocker/r-ver:4.3.1")) {
  
  mode <- match.arg(mode)
  pkg_dir <- normalizePath(pkg_dir, winslash = "/", mustWork = TRUE)
  
  if (!file.exists(file.path(pkg_dir, "DESCRIPTION"))) {
    stop("No DESCRIPTION file found in: ", pkg_dir)
  }
  
  if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools", dependencies = TRUE)
  
  cat("📦 Building package...\n")
  tarball <- devtools::build(pkg = pkg_dir, quiet = FALSE)
  pkg_file <- basename(tarball)
  
  # --------------------------------------
  # LOCAL CHECK
  # --------------------------------------
  if (mode == "local") {
    if (run_local_as_cran) {
      cat("\n🔎 Running local --as-cran check...\n")
      rcmdcheck::rcmdcheck(
        tarball,
        args = "--as-cran",
        build = FALSE
      )
    }
  }
  
  # --------------------------------------
  # SERVER MODE
  # --------------------------------------
  if (mode == "server") {
    if (is.null(server)) stop("Provide 'server' (e.g. user@host).")
    
    cat("\n🚀 Uploading to server...\n")
    system(sprintf("ssh %s 'mkdir -p %s'", server, remote_dir))
    system(sprintf("scp %s %s:%s/", tarball, server, remote_dir))
    
    remote_cmd <- sprintf(
      "cd %s && R CMD check --as-cran %s > check.log 2>&1",
      remote_dir,
      pkg_file
    )
    
    cat("🧪 Running remote check...\n")
    system(sprintf("ssh %s '%s'", server, remote_cmd))
    
    cat("⬇ Downloading log...\n")
    system(sprintf("scp %s:%s/check.log ./check.log", server, remote_dir))
    
    cat("✅ Server check complete. Log saved as check.log\n")
  }
  
  # --------------------------------------
  # WINBUILDER MODE
  # --------------------------------------
  if (mode == "winbuilder") {
    if (is.null(email)) stop("Provide 'email' for win-builder submission.")
    
    cat("\n🪟 Submitting to win-builder...\n")
    for (v in win_versions) {
      cat("   →", v, "\n")
      if (v == "release") devtools::check_win_release(pkg_dir)
      if (v == "devel") devtools::check_win_devel(pkg_dir)
      if (v == "oldrelease") devtools::check_win_oldrelease(pkg_dir)
    }
    cat("\n📬 Submitted to win-builder.\n")
  }
  
  # --------------------------------------
  # DOCKER MODE (Robust Shell Escaping)
  # --------------------------------------
  if (mode == "docker") {
    cat("\n🐳 Running fast Docker checks (using pre-built images)...\n")
    
    # 1. Paket lokal bauen
    tarball <- devtools::build(pkg_dir, quiet = FALSE)
    tmp_dir <- tempfile("pkg_docker_")
    dir.create(tmp_dir)
    source_dir <- file.path(tmp_dir, "source")
    dir.create(source_dir)
    
    # 2. Entpacken für den Mount
    utils::untar(tarball, exdir = source_dir)
    pkg_name <- list.dirs(source_dir, full.names = FALSE, recursive = FALSE)[1]
    docker_pkg_path <- paste0("/pkg/source/", pkg_name)
    
    for (img in docker_images) {
      cat("\n🚀 Testing on:", img, "\n")
      
      # Umgebungsvariablen für CRAN-Simulation
      env_flags <- "-e _R_CHECK_FORCE_SUGGESTS_=true"
      if (grepl("devel", img)) {
        env_flags <- paste(env_flags, "-e SPECIAL_DONTTEST=1")
      }
      
      # Da die Pakete im Image sind, reicht ein direkter R CMD check
      cmd <- sprintf(
        "docker run --platform=linux/amd64 --rm %s -v %s:/pkg %s R CMD check --as-cran %s",
        env_flags,
        tmp_dir,
        img,
        docker_pkg_path
      )
      
      cat("Command:", cmd, "\n")
      system(cmd)
    }
    
    cat("\n✅ Docker checks complete.\n")
  }
  
  invisible(tarball)
}

#### Setup ####

pkgs <- c("devtools", "rhub")
missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing) > 0) {
  install.packages(missing, dependencies = TRUE)
}

dir <- "/Users/thomasniemand/Library/Mobile Documents/com~apple~CloudDocs/Publikationen/Freiheitsgradebeitrag/Cutoffs/FCO/FCO"

# temp-Ordner erstellen
tmp_dir <- tempfile("FCO_build_")
dir.create(tmp_dir)

# Paketverzeichnis in temp kopieren (rekursiv)
file.copy(dir,
          tmp_dir, recursive = TRUE)

# Neuen Pfad für build/check
pkg_local <- file.path(tmp_dir, "FCO")

#### Checks ####

##### Devtools #####
devtools::check(pkg_local)

##### Lokal #####
check_package(
  pkg_dir = dir,
  mode = "local"
)

##### Win-builder #####
check_package(
  pkg_dir = pkg_local,
  mode = "winbuilder",
  email = "thomas.niemand@gmail.com",
  win_versions = c("release", "devel", "oldrelease")
)

##### Docker fuer verschiedene Versionen ######
#Docker muss installiert sein: https://www.docker.com/products/docker-desktop/
check_package(pkg_dir = pkg_local, 
              mode = "docker",
              docker_images = c("rocker/r-devel", "rocker/r-ver:4.3.1"))

#Pfad entfernen
unlink(tmp_dir, recursive = TRUE)
