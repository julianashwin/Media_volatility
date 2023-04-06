import os
os.chdir('/Users/julianashwin/Documents/GitHub/Media_volatility')

import pandas as pd
from time import sleep
import datetime
from tqdm import tqdm


matched_articles = pd.read_csv("selenium_factiva/article_info.csv")

matched_articles = matched_articles.loc[matched_articles.Date > "2000-09-01",:]
matched_articles.reset_index(inplace=True, drop = True)

matched_articles["save_time"] = ""
matched_articles["online_found"] = ""

from selenium import webdriver
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.by import By
from selenium.webdriver.chrome.options import Options

options = Options()
options.add_argument("--window-size=1920x1080")
options.add_argument("--verbose")


driver = webdriver.Chrome(options=options)


driver.get("https://www.london.edu/campaigns/it/new-portal-landing-page")
driver.get(driver.current_url)

# Select sign in here

driver.find_element(By.XPATH, '//*[@id="i0116"]').clear()
driver.find_element(By.XPATH, '//*[@id="i0116"]').send_keys("jashwin@london.edu")
driver.find_element(By.XPATH, '//*[@id="idSIButton9"]').click()

driver.find_element(By.XPATH, '//*[@id="i0118"]').send_keys("Eva.is.a.10")
driver.find_element(By.XPATH, '//*[@id="idSIButton9"]').click()

# Go to LBS portal - Library Databases - Factiva (careful not to open a new tab)
# Open the search form
driver.find_element(By.XPATH, '//*[@id="sfs"]/a').click()

# Select the sources
driver.find_element(By.XPATH, '//*[@id="scTab"]/div[1]').click()
# Manually enter the search term 

## Select the date
driver.find_element(By.XPATH, '//*[@id="dr"]').click()
# Click on Enter date range here

## Dataframe to fill

NN = len(matched_articles) 
for ii in tqdm(range(16561,NN)):
    print("\n"+str(ii)+"\n")
    # Extract the necessary info
    frdate = matched_articles.Date_lag[ii]
    todate = matched_articles.Date[ii]
    hline = matched_articles.headline[ii]
    keyphrase = matched_articles.key_phrase[ii]
    code = matched_articles.Code[ii]
    # From date
    fryr = frdate[0:4]
    frmth = frdate[5:7]
    frdy = frdate[8:10]
    # To date
    toyr = todate[0:4]
    tomth = todate[5:7]
    tody = todate[8:10]
    try:
        driver.find_element(By.XPATH, '//*[@id="frd"]').clear()
    except:
        try:
            sleep(2.5)
            driver.find_element(By.XPATH, '//*[@id="btnModifySearch"]/div/span').click()
            sleep(2)
            driver.find_element(By.XPATH, '//*[@id="frd"]').clear()
        except:
            driver.find_element(By.XPATH, '//*[@id="frd"]').clear()
    driver.find_element(By.XPATH, '//*[@id="frd"]').send_keys(frdy)
    # Month
    driver.find_element(By.XPATH, '//*[@id="frm"]').clear()
    driver.find_element(By.XPATH, '//*[@id="frm"]').send_keys(frmth)
    # Year
    driver.find_element(By.XPATH, '//*[@id="fry"]').clear()
    driver.find_element(By.XPATH, '//*[@id="fry"]').send_keys(fryr)
    ## To
    # Day
    driver.find_element(By.XPATH, '//*[@id="tod"]').clear()
    driver.find_element(By.XPATH, '//*[@id="tod"]').send_keys(tody)
    # Month
    driver.find_element(By.XPATH, '//*[@id="tom"]').clear()
    driver.find_element(By.XPATH, '//*[@id="tom"]').send_keys(tomth)
    # Year
    driver.find_element(By.XPATH, '//*[@id="toy"]').clear()
    driver.find_element(By.XPATH, '//*[@id="toy"]').send_keys(toyr)

    # Enter key phrase from mentioned orgs as search term
    driver.find_element(By.XPATH, '//*[@id="atx"]').clear()
    driver.find_element(By.XPATH, '//*[@id="atx"]').send_keys(keyphrase)

    # Press search
    driver.find_element(By.XPATH, '//*[@id="btnSBSearch"]/div/span').click()
    print("Wait for page to load")
    sleep(5)
    ## Do the saving of files here
    articles_found = "No"
    try:
        # Select all
        print("Select all articles")
        driver.find_element(By.XPATH, '//*[@id="selectAll"]/input').click()
        sleep(1)
        # Open RTF menu
        driver.find_element(By.XPATH, '//*[@id="listMenuRoot"]/li[5]/a').click()
        sleep(0.5)
        driver.find_element(By.XPATH, '//*[@id="listMenuRoot"]/li[5]/a').click()
        sleep(0.5)
        # Download as articles
        driver.find_element(By.XPATH, '//*[@id="listMenu-id-3"]/li[2]/a').click()
        sleep(3)
        articles_found = "Yes"
        print("Download successful for "+code+" around "+todate)
    except:
        print("No articles for "+code+" around "+todate)
        sleep(1)
    
    matched_articles.loc[ii, "save_time"] = str(datetime.datetime.now())
    matched_articles.loc[ii, "online_found"] = articles_found

    save_file = matched_articles.loc[matched_articles.Code == code]
    save_file.to_csv("selenium_factiva/save_times/"+code+".csv")

    # Click Modify Search to go back 
    try:
        driver.find_element(By.XPATH, '//*[@id="btnModifySearch"]/div/span').click()
    except:
        sleep(1.5)
        driver.get(driver.current_url)
        try:
            sleep(2.5)
            driver.find_element(By.XPATH, '//*[@id="btnModifySearch"]/div/span').click()
        except:
            sleep(2.5)
            driver.find_element(By.XPATH, '//*[@id="btnModifySearch"]/div/span').click()
    sleep(1.5)

