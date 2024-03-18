function[geo2]=formation(geo)

n=2;

geo2.flapped=[];
geo2.nwing=0;
geo2.nelem=[];
geo2.symetric=[];
geo2.startx=geo.startx+30;
geo2.starty=geo.starty+30;
geo2.startz=geo.startz+10;
geo2.c=[];
geo2.foil=[];

geo2.startx=[geo.startx geo2.startx]
geo2.starty=[geo.starty geo2.starty]
geo2.startz=[geo.startz geo2.startz]

geo2.nx=[]
geo2.TW=[]
geo2.dihed=[]
geo2.ny=[]
    
geo2.b=[]
geo2.T=[]
geo2.SW=[]
geo2.meshtype=[]
geo2.fc=[]
geo2.fnx=[]
geo2.fsym=[]
geo2.flap_vector=[]

for i=1:(n)
    geo2.flapped=[geo2.flapped;geo.flapped]
    geo2.nwing=geo2.nwing+geo.nwing
    geo2.nelem=[geo2.nelem geo.nelem]
    geo2.symetric=[geo2.symetric geo.symetric]
    geo2.c=[geo2.c geo.c]
    geo2.foil=[geo2.foil;geo.foil]
    geo2.nx=[geo2.nx;geo.nx]
    geo2.TW=[geo2.TW;geo.TW]
    geo2.dihed=[geo2.dihed;geo.dihed]
    geo2.ny=[geo2.ny;geo.ny]
    
    geo2.b=[geo2.b;geo.b]
    geo2.T=[geo2.T;geo.T]
    geo2.SW=[geo2.SW;geo.SW]
    geo2.meshtype=[geo2.meshtype;geo.meshtype]
    geo2.fc=[geo2.fc;geo.fc]
    geo2.fnx=[geo2.fnx;geo.fnx]
    geo2.fsym=[geo2.fsym;geo.fsym]
    geo2.flap_vector=[geo2.flap_vector;geo.flap_vector]
    
     
end