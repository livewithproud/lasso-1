import MySQLdb
import datetime

db = MySQLdb.connect(host="127.0.0.1", port=2002, user="root", passwd="welcome1", db="renet")
cursor = db.cursor()

# part days by segLen
# example: part 100 days by 10 days
def part(minDate, maxDate, segLen):
    print minDate,
    print "to",
    print maxDate
    tmpDate = minDate
    loop = 1
    minDateStr = datetime.datetime.strftime(minDate, "%Y-%m-%d")
    sets = []
    while tmpDate+datetime.timedelta(days=segLen*loop) < maxDate:
        sqlStr = "select distinct userId from analyzeuser where time > ADDDATE('%s', %d) and time < ADDDATE('%s', %d)" % (minDateStr, (loop-1)*segLen, minDateStr, loop*segLen)       
        print sqlStr
        cursor.execute(sqlStr)
        data = cursor.fetchall()
        sets.append(set(data))
        print len(data)
        loop = loop + 1
    tmpSet = set()
    if len(sets) > 0:
        tmpSet = sets[0]
        for s in sets:
            tmpSet.intersection_update(s)
    print tmpSet
    return tmpSet

def saveToFile(seg, resSet):
    with open("resData.txt", "a") as resFile:
        resFile.write("%d "%seg)
        for id in resSet:
            resFile.write("%ld\t" % id[0] )
        resFile.write("\n")
    
cursor.execute("SELECT max(time), min(time) from analyzeuser")
(maxDate, minDate) = cursor.fetchone()
for seg in [7,10,14,20,28,30,60]:
    resSet = part(minDate, maxDate, seg)
    saveToFile(seg, resSet)

db.close()
