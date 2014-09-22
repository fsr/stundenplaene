import xml.etree.ElementTree as ET
from json import JSONEncoder
import json

GermanWeekday = [(u'Mo', u'Montag'), 
                (u'Di', u'Dienstag'),
                (u'Mi', u'Mittwoch'), 
                (u'Do', u'Donnerstag'),
                (u'Fr', u'Freitag')]
Lectures = [(u'DIS+LAG', u'Mathematik 1 fr Informatiker: Diskrete Strukturen und Lineare Algebra', u'MA'),
                (u'EMI', u'Einfhrung in die Medieninformatik', u'INF'),
                (u'AuD',u'Algorithmen und Datenstrukturen', u'INF'),
                (u'TGI', u'Technische Grundlagen der Informatik', u'INF'),
                (u'RA', u'Rechnerarchitektur I', u'INF')]
Slots = [(u'7:30', u'9:00', u'1. DS'),
        (u'09:20', u'10:50', u'2. DS'),
        (u'11:10', u'12:40', u'3. DS'),
        (u'13:00', u'14:30', u'4. DS'),
        (u'14:50', u'16:20', u'5. DS'),
        (u'16:40', u'18:10', u'6. DS'),
        (u'18:30', u'20:00', u'7. DS')]
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
    def __init__(self, kind, kindShort, name, nameShort, fac, day, dayShort, slot, start, end, location, teacher):
        self.Kind = kind
        self.KindShort = kindShort
        self.Name = name
        self.NameShort = nameShort
        self.Fac = fac
        self.Day = day
        self.DayShort = dayShort
        self.Slot = slot
        self.Start = start
        self.End = end
        self.Location = location
        self.Teacher = teacher
    
def loadFileToPy(filename):
    with open (filename, "r") as myfile:
        data = myfile.read()
    udata=data.decode("utf-8")
    udata = udata.replace(u'&nbsp;', u'leer')
    udata = udata.replace(u'&amp;', u'und')
    udata = udata.replace('fr' , 'fuer')
    asciidata=udata.encode("ascii","ignore")
    asciidata += '</body></html>'
    #data = unicode(data)
    #print asciidata
    return ET.fromstring(asciidata)



    
def makeSomething(root):
    html = ' '
    for x in range(0,42,3):
        #print x
        s = root[1][x][-1][-1][0][-1][0]
        #print s
        tablecode = s.text.split()[-1]
        table = getTable(root[1][x+1], tablecode)
        html += giveTableAsHtml(table)
    output = open('output.html', 'w')
    output.write(enclosureWithHtmlBody(html))
        

def getTable(ttable, tablecode):
    row = 1
    events = []
    for x in range(5):
        events.append([None,None,None,None,None,None,None])        
    add = 1
    if ttable[1][0].get('rowspan')=='2':
        add = 2
    #print add
    for day in range(5):
        col =1
        dayrow = ttable[row]
        row += add
        #print "next"
        #print day
        for slot in range(1,8):
            #print col
            eventfield = dayrow[col]
            ##print event.text
            if eventfield.text == "leer" :
                col = col +2
            else:
                events[day][slot] = getEventFromElement(eventfield, day, slot)
                col = col +1
    return Table(tablecode, events)

def getEventFromElement(eventfield, day, slot): 
    nameNr = 1    #
    kindNr = 0
    for x in range(5):
        if Lectures[x][1] == (eventfield[1][-1][0][0].text):
            nameNr = x
            break
    for x in range(2):
        if Kinds[x][0] == eventfield[1][-1][1][0].text:
            kindNr = x
            break
    loc = eventfield[-1][-1][-1][0].text
    if (loc) is None:
        loc = ''
    teacher = eventfield[0][1][0][0].text

    newevent = Event(
        Kinds[kindNr][1],
        Kinds[kindNr][0],
        Lectures[nameNr][1],
        Lectures[nameNr][0],
        Lectures[nameNr][2],
        GermanWeekday[day][1],
        GermanWeekday[day][0],
        slot,
        Slots[slot][0],
        Slots[slot][1],
        loc,
        teacher
    )
    return newevent
    ##print newevent.Name
    ##print Lectures[newevent.Name][1]
            
def giveTableAsJson(table, filename):
    obj=MyEncoder().encode(table)
    with open(filename, 'w') as outfile:
        json.dump(obj, outfile)
       
def enclosureWithHtmlBody(htmlContent):
    return "<html xmlns='http://www.w3.org/1999/xhtml'><body>" + \
            htmlContent + \
            '</body></html>'

    
def giveTableAsHtml(table):
    html = u'<h2>' + table.Code + u'</h2>\n<table><tbody>\n\t<tr>\n\t\t<td>' + table.Code + u'</td>\n'
    for day in range(5):
        html += '\t\t<td colspan="2" class="plan_tage">' + GermanWeekday[day][1] + '</td>\n'
    html += '\t</tr>\n'
    for slot  in range(7):
        html += '\t<tr>\n\t\t<td class="plan_stunden">' + Slots[slot][2] +  \
                '</td>\n'
        #print slot
        for day in range(5):
            html += '\t\t<td colspan="2" class="plan_name">'
            if table.Events[day][slot]:
                html += table.Events[day][slot].NameShort
            html += '</td>\n'
        html += '\t</tr>\n\t<tr>\n\t\t<td class="plan_uhrzeit">' + Slots[slot][0] + ' - ' + \
                 Slots[slot][1] + '</td>\n'
        for day in range(5):
            html += '\t\t<td class="plan_dozent">'
            if table.Events[day][slot] is not None:
                html += table.Events[day][slot].Teacher
            html += '</td>\n\t\t<td class="plan_raum">'
            #print day
            if table.Events[day][slot] is not None:
                html += table.Events[day][slot].Location
            html += '</td>\n'
        html += '\t</tr>\n'
    html += '</tbody>\n</table>\n<hr style="page-break-after:always;width:0px"/>\n'
    return html

if __name__ == '__main__':
    makeSomething(loadFileToPy('140918_studentensets_org.htm'))
    #giveTableAsHtml(getTable(loadFileToString('140918_studentensets_org.htm')), 'InfBa_01.html  ')
        
      
      
  
  

