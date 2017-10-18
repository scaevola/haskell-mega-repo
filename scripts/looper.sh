#!/bin/sh

request() {
	URL=$1
	echo "Requesting $URL"
	time --portability -- curl -H "Accept: application/json" -s -D - -w 'size %{size_download}\n' -o /dev/null $URL
	echo "Requesting $URL" | sed s/./=/g
}

while true; do
	request http://localhost:8807/hours-by-task
	request http://localhost:8807/missing-hours
	request http://localhost:8807/issues
	request http://localhost:8807/power/users
	request http://localhost:8807/power/projects
	request http://localhost:8807/power/absences

	sleep 2
done
