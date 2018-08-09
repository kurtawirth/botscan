import botometer

def create_bom(mashape_key, twitter_app_auth):
  bom = botometer.Botometer(wait_on_ratelimit=True,
    mashape_key=mashape_key,
    **twitter_app_auth)
  return bom
