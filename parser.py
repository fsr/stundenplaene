import xml.etree.ElementTree as ET
from json import JSONEncoder
import json

GermanWeekday = [(u'Mo', u'Montag'), 
                (u'Di', u'Dienstag'),
                (u'Mi', u'Mittwoch'), 
                (u'Do', u'Donnerstag'),
                (u'Fr', u'Freitag')]
Lectures = [(u'DIS+LAG', u'Mathematik 1 fuer Informatiker: Diskrete Strukturen und Lineare Algebra', u'MA'),
                (u'EMI', u'Einfuehrung in die Medieninformatik', u'INF'),
                (u'AuD',u'Algorithmen und Datenstrukturen', u'INF'),
                (u'TGI', u'Technische Grundlagen der Informatik', u'INF'),
                (u'RA', u'Rechnerarchitektur I', u'INF')]
Times = [(u'7:30', u'9:00'),
                (u'09:20', u'10:50'),
                (u'11:10', u'12:40'),
                (u'13:00', u'14:30'),
                (u'14:50', u'16:20'),
                (u'16:40', u'18:10'),
                (u'18:30', u'20:00')]
Kinds = [(u'V',u'Vorlesung'),
            (u'U', u'Uebung')]

class MyEncoder(JSONEncoder):
    def default(self, o):
        return o.__dict__

class Table(object):
    def __init__(self, code, events):
        self.Code = code
        self.Events = events

class Event(object):
    def __init__(self, kind, kindShort, name, nameShort, fac, day, slot, start, end, location, teacher):
        self.Kind = kind
        self.KindShort = kindShort
        self.Name = name
        self.NameShort = nameShort
        self.Fac = fac
        self.Day = day
        self.Slot = slot
        self.Start = start
        self.End = end
        self.Location = location
        self.Teacher = teacher
    
def loadFileToString(filename):
    with open (filename, "r") as myfile:
        data=myfile.read().replace('&nbsp;', 'leer')
    return data




def getTable(xmlContent):
    root = ET.fromstring(xmlContent)
    s =root[1][0][-1][-1][0][-1][0]
    tablecode = s.text.split()[-1]
    #print tablecode
    
    ttable = root[1][1]
    col =1
    events = []
    for day in range(1,6):
        col =1
        dayrow = ttable[day]
        print "next"
        print day
        for slot in range(1,8):
            eventfield = dayrow[col]
            #print col
            #print event.text
            if eventfield.text == "yxfd" :
                col = col +2
            else:
                events.append(getEventFromElement(eventfield, day, slot))
                col = col +1
    return Table(tablecode, events)

def getEventFromElement(eventfield, day, slot):
    name = 1    #eventfield[1][-1][0][0].text
    kind = 0
    for x in range(5):
        if Lectures[x][1] == (eventfield[1][-1][0][0].text):
            nameNr = x
            break
    for x in range(2):
        if Kinds[x][0] == eventfield[1][-1][1][0].text:
            kindNr = x
            break
    loc = eventfield[-1][-1][-1][0].text
    teacher = eventfield[0][1][0][0].text

    newevent = Event(
        Kinds[kindNr][1],
        Kinds[kindNr][0],
        Lectures[nameNr][1],
        Lectures[nameNr][0],
        Lectures[nameNr][2],
        day,
        slot,
        Times[slot][0],
        Times[slot][1],
        loc,
        teacher
    )
    return newevent
    #print newevent.Name
    #print Lectures[newevent.Name][1]    
            
def giveTableAsJson(table, filename):
    obj=MyEncoder().encode(table)
    with open(filename, 'w') as outfile:
        json.dump(obj, outfile)
       

if __name__ == '__main__':
    giveTableAsJson(getTable(loadFileToString('140918_studentensets.htm')), 'InfBa_01.json')
        
      
      
  
  

