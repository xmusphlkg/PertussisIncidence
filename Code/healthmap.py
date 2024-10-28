from bs4 import BeautifulSoup
import requests
import json
import pandas as pd
from datetime import datetime

# function to get the data from the website
def extract_information(html_content):
    soup = BeautifulSoup(html_content, "lxml")
    news_items = []

    for div in soup.find_all("div", class_="at"):
        raw_date = div.find("span", class_="d").text.strip().rstrip(" -")
        formatted_date = datetime.strptime(raw_date, "%d %b %Y").strftime("%Y-%m-%d")
        
        news_title = div.find("a", class_="fbox").text.strip()

        news_items.append({
            "date": formatted_date,
            "title": news_title
        })
    
    return news_items

# setting the URL and parameters
url = "https://www.healthmap.org/getAlerts.php"
params = {
    "category[]": ["1"],
    "diseases[]": "100",
    "sdate": "01/01/2024",
    "edate": "10/28/2024",
    "heatscore": "1",
    "partner": "hm"
}

# sending the request
response = requests.get(url, params=params)

# checking the response
if response.status_code == 200:
    data = response.json()
    all_news_items = []

    for marker in data['markers']:
        news_items = extract_information(marker['html'])
        for item in news_items:
            item['place_name'] = marker['place_name']
            all_news_items.append(item)

    df = pd.DataFrame(all_news_items)
    df.to_csv("Data/HealthmapDataNew.csv", index=False)
    print("Data has been saved to 'health_news_data.csv'.")
else:
    print("Failed to retrieve data:", response.status_code)
    
    
    
    
