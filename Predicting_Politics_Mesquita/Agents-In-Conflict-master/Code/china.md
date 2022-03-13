# China Experiment

This is an attempt to replicate the China forecast from Predicting Politics


```python
import copy
from collections import defaultdict, namedtuple
from itertools import combinations
import csv

import numpy as np
import scipy.stats

import pandas as pd
import networkx as nx

import matplotlib.pyplot as plt
```


```python
from negotiation_model import *
from bdm_agent import *
```


```python
class BDMActor(NegotiationActor):
    DecisionClass = BDM_Agent
```

## Load data


```python
data = pd.read_csv("/Users/dmasad/Programming/Learning/BDM/ChinaData.csv")
```


```python
data
```




<div>
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Stakeholder</th>
      <th>Resources</th>
      <th>Position</th>
      <th>Salience</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>Zeming</td>
      <td>80</td>
      <td>25</td>
      <td>50</td>
    </tr>
    <tr>
      <th>1</th>
      <td>Lipeng</td>
      <td>90</td>
      <td>15</td>
      <td>80</td>
    </tr>
    <tr>
      <th>2</th>
      <td>Qiaoshi</td>
      <td>85</td>
      <td>15</td>
      <td>80</td>
    </tr>
    <tr>
      <th>3</th>
      <td>Shanghai</td>
      <td>30</td>
      <td>35</td>
      <td>50</td>
    </tr>
    <tr>
      <th>4</th>
      <td>Ruihuan</td>
      <td>60</td>
      <td>30</td>
      <td>75</td>
    </tr>
    <tr>
      <th>5</th>
      <td>Tianjiyun</td>
      <td>50</td>
      <td>20</td>
      <td>40</td>
    </tr>
    <tr>
      <th>6</th>
      <td>Zhurongji</td>
      <td>60</td>
      <td>35</td>
      <td>80</td>
    </tr>
    <tr>
      <th>7</th>
      <td>Zoujiahua</td>
      <td>80</td>
      <td>15</td>
      <td>75</td>
    </tr>
    <tr>
      <th>8</th>
      <td>Shangkun</td>
      <td>100</td>
      <td>20</td>
      <td>85</td>
    </tr>
    <tr>
      <th>9</th>
      <td>Baibing</td>
      <td>85</td>
      <td>15</td>
      <td>85</td>
    </tr>
    <tr>
      <th>10</th>
      <td>Wangzhen</td>
      <td>85</td>
      <td>15</td>
      <td>80</td>
    </tr>
    <tr>
      <th>11</th>
      <td>Wanli</td>
      <td>60</td>
      <td>25</td>
      <td>60</td>
    </tr>
    <tr>
      <th>12</th>
      <td>Ziannian</td>
      <td>60</td>
      <td>20</td>
      <td>60</td>
    </tr>
    <tr>
      <th>13</th>
      <td>Cheyun</td>
      <td>100</td>
      <td>10</td>
      <td>85</td>
    </tr>
    <tr>
      <th>14</th>
      <td>Boyibo</td>
      <td>75</td>
      <td>20</td>
      <td>70</td>
    </tr>
    <tr>
      <th>15</th>
      <td>Pengzhen</td>
      <td>90</td>
      <td>15</td>
      <td>80</td>
    </tr>
    <tr>
      <th>16</th>
      <td>Challdem</td>
      <td>20</td>
      <td>100</td>
      <td>90</td>
    </tr>
    <tr>
      <th>17</th>
      <td>Chspring</td>
      <td>20</td>
      <td>100</td>
      <td>90</td>
    </tr>
    <tr>
      <th>18</th>
      <td>STU/INTEL</td>
      <td>40</td>
      <td>100</td>
      <td>90</td>
    </tr>
    <tr>
      <th>19</th>
      <td>USA</td>
      <td>70</td>
      <td>100</td>
      <td>40</td>
    </tr>
    <tr>
      <th>20</th>
      <td>Japan</td>
      <td>50</td>
      <td>60</td>
      <td>50</td>
    </tr>
    <tr>
      <th>21</th>
      <td>Europe</td>
      <td>55</td>
      <td>90</td>
      <td>30</td>
    </tr>
    <tr>
      <th>22</th>
      <td>Hong Kong</td>
      <td>25</td>
      <td>100</td>
      <td>85</td>
    </tr>
    <tr>
      <th>23</th>
      <td>Guandung</td>
      <td>30</td>
      <td>50</td>
      <td>70</td>
    </tr>
  </tbody>
</table>
</div>




```python
# Normalize
#data.Resources /= data.Resources.sum()
data.Resources /= 100
data.Position = ((data.Position - data.Position.min()) 
                 / (data.Position.max() - data.Position.min()))
data.Salience /= 100
```

## Start and run model


```python
agents = []
for i, row in data.iterrows():
    agent = BDMActor(row.Stakeholder, row.Resources, row.Position, row.Salience)
    agent.decision_model.Q = 1
    agent.decision_model.T = 1
    agent.decision_model.verbose = False
    agents.append(agent)
    
model = NegotiationModel(agents)
```


```python
model.find_median()
```




    0.1111111111111111




```python
for agent in model.agents:
    print(agent)
```

    Zeming              	Position: 0.17	Capability: 0.80	Salience: 0.50
    Lipeng              	Position: 0.06	Capability: 0.90	Salience: 0.80
    Qiaoshi             	Position: 0.06	Capability: 0.85	Salience: 0.80
    Shanghai            	Position: 0.28	Capability: 0.30	Salience: 0.50
    Ruihuan             	Position: 0.22	Capability: 0.60	Salience: 0.75
    Tianjiyun           	Position: 0.11	Capability: 0.50	Salience: 0.40
    Zhurongji           	Position: 0.28	Capability: 0.60	Salience: 0.80
    Zoujiahua           	Position: 0.06	Capability: 0.80	Salience: 0.75
    Shangkun            	Position: 0.11	Capability: 1.00	Salience: 0.85
    Baibing             	Position: 0.06	Capability: 0.85	Salience: 0.85
    Wangzhen            	Position: 0.06	Capability: 0.85	Salience: 0.80
    Wanli               	Position: 0.17	Capability: 0.60	Salience: 0.60
    Ziannian            	Position: 0.11	Capability: 0.60	Salience: 0.60
    Cheyun              	Position: 0.00	Capability: 1.00	Salience: 0.85
    Boyibo              	Position: 0.11	Capability: 0.75	Salience: 0.70
    Pengzhen            	Position: 0.06	Capability: 0.90	Salience: 0.80
    Challdem            	Position: 1.00	Capability: 0.20	Salience: 0.90
    Chspring            	Position: 1.00	Capability: 0.20	Salience: 0.90
    STU/INTEL           	Position: 1.00	Capability: 0.40	Salience: 0.90
    USA                 	Position: 1.00	Capability: 0.70	Salience: 0.40
    Japan               	Position: 0.56	Capability: 0.50	Salience: 0.50
    Europe              	Position: 0.89	Capability: 0.55	Salience: 0.30
    Hong Kong           	Position: 1.00	Capability: 0.25	Salience: 0.85
    Guandung            	Position: 0.44	Capability: 0.30	Salience: 0.70



```python
model.step()
```


```python
for agent in model.agents:
    print(agent)
```

    Zeming              	Position: 0.17	Capability: 0.80	Salience: 0.50
    Lipeng              	Position: 0.06	Capability: 0.90	Salience: 0.80
    Qiaoshi             	Position: 0.06	Capability: 0.85	Salience: 0.80
    Shanghai            	Position: 0.28	Capability: 0.30	Salience: 0.50
    Ruihuan             	Position: 0.18	Capability: 0.60	Salience: 0.75
    Tianjiyun           	Position: 0.11	Capability: 0.50	Salience: 0.40
    Zhurongji           	Position: 0.22	Capability: 0.60	Salience: 0.80
    Zoujiahua           	Position: 0.06	Capability: 0.80	Salience: 0.75
    Shangkun            	Position: 0.11	Capability: 1.00	Salience: 0.85
    Baibing             	Position: 0.06	Capability: 0.85	Salience: 0.85
    Wangzhen            	Position: 0.06	Capability: 0.85	Salience: 0.80
    Wanli               	Position: 0.17	Capability: 0.60	Salience: 0.60
    Ziannian            	Position: 0.11	Capability: 0.60	Salience: 0.60
    Cheyun              	Position: 0.03	Capability: 1.00	Salience: 0.85
    Boyibo              	Position: 0.11	Capability: 0.75	Salience: 0.70
    Pengzhen            	Position: 0.06	Capability: 0.90	Salience: 0.80
    Challdem            	Position: 0.31	Capability: 0.20	Salience: 0.90
    Chspring            	Position: 0.31	Capability: 0.20	Salience: 0.90
    STU/INTEL           	Position: 0.31	Capability: 0.40	Salience: 0.90
    USA                 	Position: 0.38	Capability: 0.70	Salience: 0.40
    Japan               	Position: 0.53	Capability: 0.50	Salience: 0.50
    Europe              	Position: 0.72	Capability: 0.55	Salience: 0.30
    Hong Kong           	Position: 0.32	Capability: 0.25	Salience: 0.85
    Guandung            	Position: 0.29	Capability: 0.30	Salience: 0.70



```python
model.find_median()
```




    0.1111111111111111




```python
for agent in model.agents:
    print(agent)
    print(agent.decision_model.action_log)
    print("")
```

    Zeming              	Position: 0.17	Capability: 0.80	Salience: 0.50
    ['Zeming conflict with Europe']
    
    Lipeng              	Position: 0.06	Capability: 0.90	Salience: 0.80
    ['Lipeng conflict with Tianjiyun']
    
    Qiaoshi             	Position: 0.06	Capability: 0.85	Salience: 0.80
    ['Qiaoshi conflict with Tianjiyun']
    
    Shanghai            	Position: 0.28	Capability: 0.30	Salience: 0.50
    ['Shanghai conflict with Cheyun']
    
    Ruihuan             	Position: 0.18	Capability: 0.60	Salience: 0.75
    ['Ruihuan compromise with Lipeng']
    
    Tianjiyun           	Position: 0.11	Capability: 0.50	Salience: 0.40
    ['Tianjiyun conflict with Europe']
    
    Zhurongji           	Position: 0.22	Capability: 0.60	Salience: 0.80
    ['Zhurongji compromise with Lipeng']
    
    Zoujiahua           	Position: 0.06	Capability: 0.80	Salience: 0.75
    ['Zoujiahua conflict with Tianjiyun']
    
    Shangkun            	Position: 0.11	Capability: 1.00	Salience: 0.85
    ['Shangkun conflict with Lipeng']
    
    Baibing             	Position: 0.06	Capability: 0.85	Salience: 0.85
    ['Baibing conflict with Shangkun']
    
    Wangzhen            	Position: 0.06	Capability: 0.85	Salience: 0.80
    ['Wangzhen conflict with Tianjiyun']
    
    Wanli               	Position: 0.17	Capability: 0.60	Salience: 0.60
    ['Wanli conflict with Lipeng']
    
    Ziannian            	Position: 0.11	Capability: 0.60	Salience: 0.60
    ['Ziannian conflict with Lipeng']
    
    Cheyun              	Position: 0.03	Capability: 1.00	Salience: 0.85
    ['Cheyun compromise with Tianjiyun']
    
    Boyibo              	Position: 0.11	Capability: 0.75	Salience: 0.70
    ['Boyibo conflict with Lipeng']
    
    Pengzhen            	Position: 0.06	Capability: 0.90	Salience: 0.80
    ['Pengzhen conflict with Tianjiyun']
    
    Challdem            	Position: 0.31	Capability: 0.20	Salience: 0.90
    ['Challdem compromise with Shanghai']
    
    Chspring            	Position: 0.31	Capability: 0.20	Salience: 0.90
    ['Chspring compromise with Shanghai']
    
    STU/INTEL           	Position: 0.31	Capability: 0.40	Salience: 0.90
    ['STU/INTEL compromise with Shanghai']
    
    USA                 	Position: 0.38	Capability: 0.70	Salience: 0.40
    ['USA compromise with Ruihuan']
    
    Japan               	Position: 0.53	Capability: 0.50	Salience: 0.50
    ['Japan compromise with Cheyun']
    
    Europe              	Position: 0.72	Capability: 0.55	Salience: 0.30
    ['Europe compromise with Cheyun']
    
    Hong Kong           	Position: 0.32	Capability: 0.25	Salience: 0.85
    ['Hong Kong compromise with Shanghai']
    
    Guandung            	Position: 0.29	Capability: 0.30	Salience: 0.70
    ['Guandung compromise with Cheyun']
    


### With position changes


```python
class BDM_Agent_Moves_On_Challenge(BDM_Agent):
    def lose_conflict(self, winner):
        self.position = winner.position

class BDM_Moving_On_Conflict_Actor(NegotiationActor):
    DecisionClass = BDM_Agent_Moves_On_Challenge

class NegotiationModel_DeterministicWinner(NegotiationModel):
    def resolve_conflict(self, side_1, side_2):
        eu_1 = side_1.decision_model.compute_eu(side_1, side_2)
        eu_2 = side_2.decision_model.compute_eu(side_2, side_1)
        if eu_1 > eu_2:
            return True
        else:
            return False
```


```python
agents = []
for i, row in data.iterrows():
    agent = BDM_Moving_On_Conflict_Actor(row.Stakeholder, 
                                         row.Resources, row.Position, row.Salience)
    agent.decision_model.Q = 0.5
    agent.decision_model.T = 0.5
    agent.decision_model.verbose = False
    agents.append(agent)
    
model = NegotiationModel_DeterministicWinner(agents)
```


```python
model.step()
```


```python
model.find_median()
```




    0.1111111111111111




```python
for agent in model.agents:
    print(agent)
```

    Zeming              	Position: 0.17	Capability: 0.80	Salience: 0.50
    Lipeng              	Position: 0.06	Capability: 0.90	Salience: 0.80
    Qiaoshi             	Position: 0.06	Capability: 0.85	Salience: 0.80
    Shanghai            	Position: 0.28	Capability: 0.30	Salience: 0.50
    Ruihuan             	Position: 0.17	Capability: 0.60	Salience: 0.75
    Tianjiyun           	Position: 0.11	Capability: 0.50	Salience: 0.40
    Zhurongji           	Position: 0.19	Capability: 0.60	Salience: 0.80
    Zoujiahua           	Position: 0.06	Capability: 0.80	Salience: 0.75
    Shangkun            	Position: 0.11	Capability: 1.00	Salience: 0.85
    Baibing             	Position: 0.06	Capability: 0.85	Salience: 0.85
    Wangzhen            	Position: 0.06	Capability: 0.85	Salience: 0.80
    Wanli               	Position: 0.17	Capability: 0.60	Salience: 0.60
    Ziannian            	Position: 0.11	Capability: 0.60	Salience: 0.60
    Cheyun              	Position: 0.06	Capability: 1.00	Salience: 0.85
    Boyibo              	Position: 0.11	Capability: 0.75	Salience: 0.70
    Pengzhen            	Position: 0.06	Capability: 0.90	Salience: 0.80
    Challdem            	Position: 0.30	Capability: 0.20	Salience: 0.90
    Chspring            	Position: 0.30	Capability: 0.20	Salience: 0.90
    STU/INTEL           	Position: 0.30	Capability: 0.40	Salience: 0.90
    USA                 	Position: 0.34	Capability: 0.70	Salience: 0.40
    Japan               	Position: 0.38	Capability: 0.50	Salience: 0.50
    Europe              	Position: 0.32	Capability: 0.55	Salience: 0.30
    Hong Kong           	Position: 0.31	Capability: 0.25	Salience: 0.85
    Guandung            	Position: 0.23	Capability: 0.30	Salience: 0.70



```python
model.agent_names["USA"].decision_model.action_log
```




    ['USA compromise with Ruihuan', 'USA compromise with Cheyun']




```python
model.find_mean()
```




    0.13234238349526278


