library(googleCloudRunner)

cr_deploy_docker_trigger_with_secret <- function (repo, image, trigger_name = paste0("docker-", image), 
                                                  image_tag = c("latest", "$SHORT_SHA", "$BRANCH_NAME"), ..., 
                                                  substitutions = NULL, ignoredFiles = NULL, includedFiles = NULL, 
                                                  timeout = NULL, projectId_target = cr_project_get(),
                                                  buildstep_secret = NULL) 
{
  build_docker <- cr_build_make(cr_build_yaml(steps = c(buildstep_secret, cr_buildstep_docker(image, 
                                                                            tag = image_tag, projectId = projectId_target, ..., 
                                                                            kaniko_cache = TRUE)), timeout = timeout))
  safe_name <- gsub("[^a-zA-Z1-9]", "-", trigger_name)
  cr_buildtrigger(build_docker, name = safe_name, trigger = repo, 
                  description = paste0(safe_name, Sys.time()), trigger_tags = "docker-build", 
                  substitutions = substitutions, ignoredFiles = ignoredFiles, 
                  includedFiles = includedFiles)
}


repo <- cr_buildtrigger_repo("neilc-itv/barbBrowser")

# deploy a cloud build trigger so each commit build the image
cr_deploy_docker_trigger_with_secret(
  repo,
  image = "barb-browser",
  trigger_name = "docker-barb-browser",
  buildstep_secret = cr_buildstep_secret("barb-api", decrypted = "/workspace/auth.json")
)

# deploy to Cloud Run
cr_run(sprintf("gcr.io/%s/barb-browser:latest",cr_project_get()),
       port = 8080,
       name = "barb-browser",
       concurrency = 80,
       max_instances = 1)


