library(googleCloudRunner)

repo <- cr_buildtrigger_repo("neilc-itv/barbBrowser")

# deploy a cloud build trigger so each commit build the image
cr_deploy_docker_trigger(
  repo,
  image = "barb-browser"
)

# deploy to Cloud Run
cr_run(sprintf("gcr.io/%s/barb-browser:latest",cr_project_get()),
       port = 3838,
       name = "barb-browser",
       concurrency = 80,
       max_instances = 1)

