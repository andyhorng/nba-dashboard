
image := asia.gcr.io/dadog-2016/nba-dashboard

.PHONY: build push

build:
	docker build -t $(image) .

push: build
	gcloud docker push $(image)

