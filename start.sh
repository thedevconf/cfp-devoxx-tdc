#!/bin/bash
echo "--- Starting CFP ---"
echo "--- CFP: $CFP_CONFCODE"
echo "--- Instance: $EC2_ID"
echo "--- REDIS: $REDIS_HOST"

sbt -mem 1024 start
