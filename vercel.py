import requests
import datetime
import os
import argparse
 

api_url = "https://api.vercel.com"


def delete_old_previews(token):
    old_previews = []
    newer_previews = {}
    preview_count = 0
    deleted_preview = 0

    s = requests.Session()
    s.headers.update({'Authorization': f'Bearer {token}'})

    # get all of the deployments
    response = s.get(api_url + "/v6/deployments") 
    if response.status_code != 200:
        raise Exception("Sorry exit status of api request is not OK: exit status ", response.status_code)

    data = response.json()

    # get all of the deployment IDs of deployments that are previews and are more than 6 hours old
    for deployment in data["deployments"]:
        if deployment["target"] == None:
           preview_count += 1
           time_in_s = deployment["created"] / 1000
           date_time = datetime.datetime.fromtimestamp(time_in_s)
           current_time = datetime.datetime.now()
           time_difference = current_time - date_time
           if time_difference > datetime.timedelta(hours=24):
                old_previews += [deployment["uid"]]
           else:
                newer_previews[deployment["uid"]] = time_difference

    # remove all the old deployments
    for preview in old_previews:
        deleted_preview += 1
        response = s.delete(api_url + "/v13/deployments/" + preview)
        if response.status_code != 200:
            raise Exception("Sorry exit status of api request is not OK: exit status ", response.status_code)
    
    active_previews = preview_count - deleted_preview

    # if there are still more than 10 active preview deployments and delete the oldest ones until there are only 9 active deployments
    while active_previews >= 15:
        time_differences = list(newer_previews.values())
        oldest_creation = max(time_differences)
        deployment_id = list(newer_previews.keys())[time_differences.index(oldest_creation)]
        response = s.delete(api_url + "/v13/deployments/" + deployment_id)
        if response.status_code != 200:
            raise Exception("Sorry exit status of api request is not OK: exit status ", response.status_code)
        active_previews -= 1
        del newer_previews[deployment_id]


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        description="Script removes old preview deployments"
    )
    parser.add_argument("--token", required=True, type=str)
    args = parser.parse_args()
    token = args.token
    delete_old_previews(token)
