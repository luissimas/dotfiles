from selenium import webdriver
from selenium.common.exceptions import TimeoutException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from selenium.common.exceptions import TimeoutException
import re
import sys

import time

class Result:
    def __init__(self, url, title, content):
        self.url = url


def scrapePage(query):
    # Setting up driver
    options = webdriver.ChromeOptions()
    options.binary_location = "/usr/bin/brave"
    options.add_argument("--headless")
    driver = webdriver.Chrome(executable_path="/usr/bin/chromedriver", options=options)

    driver.get('https://www.youtube.com')

    searchBox = driver.find_element_by_xpath('/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[2]/ytd-searchbox/form/div/div[1]/input')
    searchBox.send_keys(query)

    searchButton = driver.find_element_by_xpath('/html/body/ytd-app/div/div/ytd-masthead/div[3]/div[2]/ytd-searchbox/form/button')
    searchButton.click()

    try:
        WebDriverWait(driver, 3).until(expected_conditions.presence_of_element_located((By.XPATH, '/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[1]/div[1]/div/div[1]/div/h3/a')))

        for i in range(1, 10):
            resultTitle = driver.find_element_by_xpath('/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[' + str(i) + ']/div[1]/div/div[1]/div/h3/a')
            resultChanel = driver.find_element_by_xpath('/html/body/ytd-app/div/ytd-page-manager/ytd-search/div[1]/ytd-two-column-search-results-renderer/div/ytd-section-list-renderer/div[2]/ytd-item-section-renderer/div[3]/ytd-video-renderer[' + str(i) + ']/div[1]/div/div[2]/ytd-channel-name')
            print(resultChanel.text + ' => ' + resultTitle.text + ' => ' + resultTitle.get_attribute('href'))

    except TimeoutException as error:
        print("Can't load page in time: " + str(error))

    driver.close()


    #return Result(resultUrl, resultTitle, resultContent)

scrapePage(str(sys.argv[1]))


