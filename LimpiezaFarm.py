import pandas as pd
df = pd.read_excel('farmacias.xlsx')

farmacias_clave = ['Guadalajara','Benavides','del Ahorro','San Pablo','Klyn','Farmatodo','Superfarmacias el Fénix']
regex = '|'.join(farmacias_clave)
filtro=df[df['ESTABLECIMIENTO'].str.contains(regex,case=False,na=False)]

filtro.to_excel('farmacias_filtradas.xlsx',index=False)
