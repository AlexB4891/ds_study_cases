# In command line run: pip install "bs4"
# and: pip install beautifulsoup4 requests

from bs4 import BeautifulSoup
import requests

url = "https://www.eecs.mit.edu/people/faculty-advisors"

req = requests.get(url)

soup = BeautifulSoup(req.content,features="html.parser")

print(soup.prettify)

field = soup.find_all(class_ = "field-content card-title")

names = []

for i in field:
  names.append(i.get_text())

string = "https://arxiv.org/search/?query="

string_2 = "&searchtype=all&source=header" 

urls = []

for i in names:
  urls.append(string+i+string_2)
  




for idx_1,url in enumerate(urls):
  
  req = requests.get(url)
  
  soup = BeautifulSoup(req.content,"html.parser")
    
  
  field = soup.find_all(class_="abstract-full")
  
  for idx_2,val in enumerate(field):
  
    name = "./python_R/scripts/scrapped/file_%s_%s.txt" %(idx_1,idx_2)
      
    file = open(name,"wb")
      
    text = val.get_text()
      
    file.write(text.encode("utf-8"))
      
    file.close()
    


  
