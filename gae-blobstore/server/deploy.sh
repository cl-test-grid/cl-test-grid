#!/bin/bash

~/unpacked/google-cloud-cli-396.0.0-linux-x86_64/google-cloud-sdk/bin/gcloud app deploy --project=cl-test-grid --version=21 --no-promote --no-stop-previous-version war/WEB-INF/appengine-web.xml
