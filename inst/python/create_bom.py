import botometer

#Use BotOMeter Pro API

botometer_api_url = 'https://botometer-pro.p.mashape.com'

#Reference API URL of BotOMeter Pro

def create_bom(mashape_key, twitter_app_auth):
  bom = botometer.Botometer(botometer_api_url=botometer_api_url, wait_on_ratelimit=True,
    mashape_key=mashape_key,
    **twitter_app_auth)
  return bom
