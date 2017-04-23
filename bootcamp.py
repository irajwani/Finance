#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Apr 16 14:48:56 2017

@author: imadrajwani
"""

#how to make a function
def someFunction():
 a=1;
 b=2;
 if (a<b):
     print(a)
 else: print(b)
#in the event that you want to run this function as a program
if __name__=="__someFunction__": someFunction()
print('me too')
#function with multiple arguments that uses for loop simply add each of 
#the arguments, and uses one conditional statement to condense number of else,
#elif and if
def multiargs(*args):
 result=0;
 s='';
 for x in args:
   result=result+x
   s='high' if (result>2) else 'low'
 print(result)
 print(s)
multiargs(2,5,3,8,5)
#while loop to add numbers
def loops(num1,num2):
 x=0;
 while(x<4):
  print(num1+num2)
  x=x+1
loops(3,5)
#range function creates a list similar to the JS 'for' loop's counter arguments
list(range(5,1,-1))
#you can use enumerate to assign indices to values
days=['Monday','Tuesday','Wednesday'];
fruits = ['orange', 'apple', 'pear', 'banana', 'kiwi', 'apple', 'banana'];
fruits.reverse();
fruits.index('apple',2);
fruits.sort();
fruitscopy=fruits.copy(); 
eliminatefruits=fruits.pop();
print(fruits);
print(fruitscopy);
print(eliminatefruits);                          
stack=[3,4,5];
stack.append(7);
stack.append(98);
print(stack); 
from collections import deque  
queue = deque(["Eric", "John", "Michael"]);
queue.append("Graham");
queue.popleft();
cubes= [];
for x in range(10):
    cubes.append(x**3);
print(cubes);              
#mapping one set to another given a condition     
xnotequaltoy=[(x, y) for x in [1,2,3] for y in [3,1,4] if x != y]  
print(xnotequaltoy);    
for i,d in enumerate(days):
    print(i),print(d);
#lets create our first class to be used as programs later
class firstClass():
 def diophantine(self,b,c):
  a=((b^2)+(c^2))^(0.5);
  print(a);
def testClasses():
 f=firstClass();
 testresult=f.diophatine(3,5);
 print(testresult);
if __name__=="__testClasses__":testClasses()
#to use modules from other classes to tell times and dates
from datetime import date
from datetime import datetime
from datetime import timedelta
today=date.today()
print(today)
print(today.year);
print(datetime.time(datetime.now()))
now=datetime.now()
print(now.strftime('%A,%d %B,%Y'))
print(now.strftime('%I:%M:%S %d'))
oneyearfromnow=now+timedelta(days=365);
print(oneyearfromnow);
aprilfools=date(today.year,4,1);
aprilfools=aprilfools.replace(year=today.year+1);
print(aprilfools)
#similarly for calendars
import calendar
cal=calendar.TextCalendar(calendar.MONDAY);
str=cal.formatmonth(2017,7,0,0);
print(str)
#working with files
def filewriter():
 opensesame=open("sometextfile.txt","w+");
 opensesame=open("sometextfile.txt","a+");
 for i in range(5):
  opensesame.write("This is line %d\r\n" % (i+1))
filewriter();
def filereader():
 opensesame=open("sometextfile.txt","r");
 content=opensesame.read();
 print(content);
filereader();
import shutil
from os import path
from zipfile import ZipFile
def pathchecker():
 #make a duplicate of the file   
 if path.exists("sometextfile.txt"):
  #get the path to the current directory
  src=path.realpath("sometextfile.txt");
  #seprate the path part from the filename
  head,tail=path.split(src);
  print("path is:"+head);
  print("file is:"+tail);
  #making a backup copy
  dst=src+".bak";
  shutil.copy(src,dst);
  #now put things into a zipfile
  shutil.make_archive("archive","zip",head)
  with ZipFile("testzip.zip","w") as newzip:
   newzip.write("sometextfile.txt")
pathchecker();
#fetching data from the internet using data on global seismic activity
import urllib.request
import ujson
def webfetch():
 linkedin=urllib.request.urlopen("https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/2.5_day.geojson")
 website=linkedin.read();
 status_code=linkedin.getcode()
 print(status_code);
 data=ujson.loads(website);
 print(data["metadata"]["title"]);
 for i in data["features"]:
  print(i["properties"]["place"])
  print(i["properties"]["mag"])
webfetch();
#######################
#######################
#######################
###DJANGO##############      
#step 1: !django-admin startproject mysite  
#step 2: !python manage.py runserver
#step3: !python manage.py startapp imadpoll
# Create your views here.
from django.http import HttpResponse;
def index(request):
    return HttpResponse("Hello, world. You're at the polls index.")
#now mapping the view t-o a url, first for newly created urls.py in imadpolls
from django.conf.urls import url

from . import views

urlpatterns = [
    url(r'^$', views.index, name='index'),
]
#now modify your preexisting urls.py in mysite such that it looks like:
from django.conf.urls import url
from django.conf.urls import include
from django.contrib import admin
from polls import views
urlpatterns = [
    url(r'^$', views.index, name='index'),
    url(r'^polls/', include('polls.urls')),
    url(r'^admin/', admin.site.urls)]




    

    
