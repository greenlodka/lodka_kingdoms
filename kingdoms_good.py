import pandas as pd

kingdoms = pd.read_csv('kingdoms_variable.csv', sep = ',')

name = kingdoms.Name
lat = kingdoms.lat
lng = kingdoms.lng

d = [{name[i]:[lat[i], lng[i]]} for i in range(43)]
#получаем список словаря, в котором key = княжества, а values = координаты

final_d = {}
for [(i, [k, v])] in [d[i].items() for i in range(len(d))]:
    final_d[(k, v)] = final_d.get((k, v), []) + [i]
#получаем словарь, в котором key = координаты, а values = княжества. По сути, перевернутый d

repeat = []
for value in final_d.values():
    if len(value) > 1:
        repeat.append(value)
#получаем многомерный список из имен княжеств, которые на одних и тех же местах
#print("Я получила многомерный список:", '\n', repeat) #списки имен княжеств >1 ## многомерные списки

print("I received a multidimensional list in Python:")
for row in repeat:
    print(' '.join(map(str, row)))

print("The number of kingdoms that changed their names is equal to ", len(repeat))

#for coord in final_d:
#    if final_d[coord] in repeat:
#        print(final_d[coord], 'расположены по координатам',coord)

    #if place in repeat:
        #print(final_d[place], "расположены по координатам", place)

