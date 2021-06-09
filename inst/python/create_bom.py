import botometer

# Use BotOMeter Pro API

# botometer_api_url = 'https://botometer-pro.p.rapidapi.com'

# Reference API URL of BotOMeter Pro

def create_bom(rapidapi_key, twitter_app_auth):
  bom = botometer.Botometer(wait_on_ratelimit=True,
    rapidapi_key=rapidapi_key,
    **twitter_app_auth)
  return bom
