import dash
import dash_core_components as dcc
import dash_html_components as html
import dash_table
from dash.dependencies import Input, Output
from textwrap import dedent
import matplotlib.pyplot as plt
from math import pi

from io import BytesIO
import base64

from sklearn import neighbors

import pandas as pd

external_stylesheets = ['https://codepen.io/chriddyp/pen/bWLwgP.css']
app = dash.Dash(__name__, external_stylesheets=external_stylesheets,  static_folder='data')
server = app.server

df = pd.read_csv('data/dataFIFA.csv')
df.drop(['Unnamed: 0'], axis=1, inplace=True)

#Model building
model_variables_test = ['Overall', 'Wage']
dic_dos = {'Position': {'LS' : 0,'ST' : 1,'RS' : 2,'LW' : 3,'LF' : 4,'CF' : 5,'RF' : 6,'RW': 7,'LAM': 8,'CAM':9,'RAM':10,'LM':11,
                        'LCM' : 12,'CM': 13,'RCM' : 14,'RM': 15,'LWB':16,'LDM':17,'CDM':18,'RDM':19,'RWB':20,
                        'LB' : 21,'LCB' :22,'CB' : 23,'RCB' : 24,'RB' : 25, 'GK' : 26}}

df.replace(dic_dos, inplace=True)

df.Position.fillna(27, inplace=True)
df['Position'].astype(int)

X = df[model_variables_test]
y = df.Position

X.fillna(0, inplace=True)

n_neighbors = 6
clf = neighbors.KNeighborsClassifier(n_neighbors, weights='distance')
clf.fit(X, y)

##Inicializacion de variables
dataClass = 0
posicion= 0
pos_int = 0
newDf = pd.DataFrame()
#Overall, Wage
misVar = ['LS','ST','RS','LW','LF','CF','RF','RW','LAM','CAM','RAM','LM','LCM','CM','RCM',
                   'RM','LWB','LDM','CDM','RDM','RWB','LB','LCB','CB','RCB','RB', 'GK']

abilities = ['Crossing','Finishing','HeadingAccuracy','ShortPassing','Volleys',
             'Dribbling','Curve','FKAccuracy','LongPassing','BallControl',
             'Acceleration','SprintSpeed','Agility','Reactions','Balance',
             'ShotPower','Jumping','Stamina','Strength','LongShots',
             'Aggression','Interceptions','Positioning','Vision','Penalties',
             'Composure','Marking','StandingTackle','SlidingTackle']

abilities_goalKeeper = ['GKDiving', 'GKHandling', 'GKKicking', 'GKPositioning', 'GKReflexes']
             

def fig_to_uri(in_fig, close_all=True, **save_args):
    # type: (plt.Figure) -> str
    """
    Save a figure as a URI
    :param in_fig:
    :return:
    """
    out_img = BytesIO()
    in_fig.savefig(out_img, format='png', **save_args)
    if close_all:
        in_fig.clf()
        plt.close('all')
    out_img.seek(0)  # rewind file
    encoded = base64.b64encode(out_img.read()).decode("ascii").replace("\n", "")
    return "data:image/png;base64,{}".format(encoded)

def gen_good_table(df):
    return dash_table.DataTable(
    id='table',
    columns=[{"name": i, "id": i} for i in df.columns[[1, 6, 7, 11, 87]]],
    data=df.to_dict("rows"),
    style_table={
                 'maxHeight': '350',
                 'overflowY': 'scroll',
                 'border': 'thin lightgrey solid'},
    style_cell={'width': '50px'},
    )
    
def generate_predict_dataframe(input_value):
    ov = df[df.Name == input_value].Overall.iloc[0]
    wa = df[df.Name == input_value].Wage.iloc[0]
    dataClass = clf.predict([[ov,wa]])
    
    #Vamos a establecer los filtros de salario y posicion
    condition = df[df.Name == input_value].Wage.iloc[0]
    
    if(condition > 280):
        condition = df[df.Name == input_value].Wage.iloc[0] + 50
    elif (condition > 200):
        condition = df[df.Name == input_value].Wage.iloc[0] + 25
    elif (condition > 100):
        condition = df[df.Name == input_value].Wage.iloc[0] + 15
        
    filtro = df.Wage <= condition
    filtro2 = df.Position == dataClass[0]
    
    na_me = df[(filtro & filtro2)]['Name'].iloc[0:10]
    over_all = df[(filtro & filtro2)]['Overall'].iloc[0:10]
    poten_tial = df[(filtro & filtro2)]['Potential'].iloc[0:10]
    wa_ge = df[(filtro & filtro2)]['Wage'].iloc[0:10]
    rel_clause = df[(filtro & filtro2)]['Release Clause'].iloc[0:10]
    newDf = pd.DataFrame({'Name' : na_me, 'Overall' : over_all, 'Potential' : poten_tial, 'Wage' : wa_ge, 'Release Clause' : rel_clause})
    return dash_table.DataTable(
            id='table_pred',
            columns=[{"name": i, "id": i} for i in newDf.columns[[0, 1, 2, 3, 4]]],
            data=newDf.to_dict("rows"),
            style_table={
                 'maxHeight': '350',
                 'overflowY': 'scroll',
                 'border': 'thin lightgrey solid'},
            style_cell={'width': '150px'},
            )

def generate_radar_chart(input_value):
    
    ov = df[df.Name == input_value].Overall.iloc[0]
    wa = df[df.Name == input_value].Wage.iloc[0]
    dataClass = clf.predict([[ov,wa]])
    
    #Vamos a establecer los filtros de salario y posicion
    condition = df[df.Name == input_value].Wage.iloc[0]
    
    if(condition > 280):
        condition = df[df.Name == input_value].Wage.iloc[0] + 50
    elif (condition > 200):
        condition = df[df.Name == input_value].Wage.iloc[0] + 25
    elif (condition > 100):
        condition = df[df.Name == input_value].Wage.iloc[0] + 15
    
    filtro = df.Wage <= condition
    filtro2 = df.Position == dataClass[0]
    
    na_me = df[(filtro & filtro2)]['Name'].iloc[0:5]
    newDf_names = pd.DataFrame({'Name' : na_me})
    df.drop_duplicates(subset='Name', keep="first", inplace=True)
    #We have to choose first 5 players based on KNN algo
    First = df[abilities][df.Name == newDf_names.Name.iloc[0]]
    Second = df[abilities][df.Name == newDf_names.Name.iloc[1]]
    Third = df[abilities][df.Name == newDf_names.Name.iloc[2]]
    Fourth = df[abilities][df.Name == newDf_names.Name.iloc[3]]
    Five = df[abilities][df.Name == newDf_names.Name.iloc[4]]
    
    if (dataClass[0] == 26):
        First = df[abilities_goalKeeper][df.Name == newDf_names.Name.iloc[0]]
        Second = df[abilities_goalKeeper][df.Name == newDf_names.Name.iloc[1]]
        Third = df[abilities_goalKeeper][df.Name == newDf_names.Name.iloc[2]]
        Fourth = df[abilities_goalKeeper][df.Name == newDf_names.Name.iloc[3]]
        Five = df[abilities_goalKeeper][df.Name == newDf_names.Name.iloc[4]]
    
    graph = First.append([Second, Third, Fourth, Five])
    
    #Radar Chart
    temp = 0
    #Just in case we have a goalkeeper
    if (dataClass[0] == 26):
        temp = graph.values.reshape((5, 5))
    else:
        temp= graph.values.reshape((5, 29))

    tmp = 0
    if (dataClass[0] == 26):
        tmp = pd.DataFrame(temp, columns = abilities_goalKeeper)
    else:
        tmp = pd.DataFrame(temp, columns = abilities)
    
    Attributes =list(tmp)
    AttNo = len(Attributes)
    
    values = tmp.iloc[0].tolist() #
    values += values [:1]
    
    angles = [n / float(AttNo) * 2 * pi for n in range(AttNo)]
    angles += angles [:1]
    
    values2 = tmp.iloc[1].tolist() # 
    values2 += values2 [:1]
    
    angles2 = [n / float(AttNo) * 2 * pi for n in range(AttNo)]
    angles2 += angles2 [:1]
    
    values3 = tmp.iloc[2].tolist() # 
    values3 += values3 [:1]
    
    angles3 = [n / float(AttNo) * 2 * pi for n in range(AttNo)]
    angles3 += angles3 [:1]
    
    values4 = tmp.iloc[3].tolist() # 
    values4 += values4 [:1]
    
    angles4 = [n / float(AttNo) * 2 * pi for n in range(AttNo)]
    angles4 += angles4 [:1]
    
    values5 = tmp.iloc[4].tolist() # 
    values5 += values4 [:1]
    
    angles5 = [n / float(AttNo) * 2 * pi for n in range(AttNo)]
    angles5 += angles4 [:1]
    
    plt.figure(figsize=(10,10))
    ax = plt.subplot(111, polar=True)
    
    plt.xticks(angles[:-1],Attributes)
    
    #
    ax.plot(angles, values, color = 'r')
    ax.fill(angles, values, 'red', alpha=0.1)
    
    # 
    ax.plot(angles2, values2, color = 'b')
    ax.fill(angles2, values2, 'blue', alpha=0.1)
    
    # 
    ax.plot(angles3, values3, color = 'g')
    ax.fill(angles3, values3, 'green', alpha=0.1)
    
    # 
    ax.plot(angles4, values4, color = 'orange')
    ax.fill(angles4, values4, 'orange', alpha=0.1)
    
    # 
    ax.plot(angles5, values5, color = 'black')
    ax.fill(angles5, values5, 'black', alpha=0.1)
    
    plt.figtext(0.5,0.73,newDf_names.Name.iloc[0],color='red',fontsize=20)
    plt.figtext(0.5,0.69,newDf_names.Name.iloc[1],color='blue',fontsize=20)
    plt.figtext(0.5,0.65,newDf_names.Name.iloc[2],color='green',fontsize=20)
    plt.figtext(0.5,0.61,newDf_names.Name.iloc[3],color='orange',fontsize=20)
    plt.figtext(0.5,0.57,newDf_names.Name.iloc[4],color='black',fontsize=20)
    #plt.show()
    return(plt)


colors = {
    'background': '#111111',
    'text': '#7FDBFF'
}

app.layout = html.Div([
    
    dcc.Tabs(id="tabs", children=[
        dcc.Tab(label='Introduction', children=[
            html.Div([
                dcc.Markdown(dedent('''
                ### Purpose
                &nbsp;
                
                The aim of this Dash is to build a **K-NN Model** using a Fifa 2019 dataset found on:
                * [Fifa 2019 Dataset](https://www.kaggle.com/karangadiya/fifa19)
                                 
                The Nearest Neighbors algorithm will be used to find the top 10 substitutes for a chosen player.
                
                The top 5 will be represented on a radar chart based on some attributes depending on the field position.
                
                ### Authors
                &nbsp;
                
                This Dash has been programmed by **Alvaro Ferro** and **Luis Llera** as part of the final project for Machine Learning.
                
                To know more about the authors please visit *Authors* tabs.
                
                Enjoy!
                '''))
            ])
        ]),
        dcc.Tab(label='K-NN Model', children=[
                html.Div([
                html.H4(children='K-NN Fifa 2019 Dataset'),
    
                html.Label('Choose a player:'),
                dcc.Dropdown(id='dropdown', options=[
                        {'label': i, 'value': i} for i in df.Name
                        ], multi=False, placeholder='Filter by player...'),
                html.H4('Players predicted for user selection'),
                html.Div(id = 'table_predict'),
                html.Div(id='table-container'),
    
                #Vamos a sacar el overall del jugador seleccionado
                dcc.Markdown(id='overall'),
                dcc.Markdown(id='wage'),
                dcc.Markdown(id ='position'),
                dcc.Markdown(id ='relcla'),
                
                html.Div([html.Img(id = 'radar-chart', src = '')], id='plot_div')
            ])
        ]),
    
        dcc.Tab(label='Authors', style = {}, children=[
                html.Div([
                    dcc.Markdown(dedent('''
                    &nbsp;
                    
                    **Álvaro Ferro**: 
                        
                    Graduated in Business Administration by [University of Granada](http://fccee.ugr.es/)
                    
                    *2013-2017*
                    
                    Master in Data Science for Finance by [CUNEF](https://www.cunef.edu/web/master-en-data-science-para-finanzas)
                    
                    *2018-2019*
                    
                    Programming Languages:
                        
                    * C++ 
                    * C#
                    * R (including Shiny)
                    * Python (including Dash)
                    * SQL
                    * Pyspark
                    
                    &nbsp;
                    [Github](https://github.com/AlvaroFerro)
                    
                    '''))
                ], style={'width': '33%', 'display': 'inline-block'}),
                        
                        html.Div([
                    dcc.Markdown(dedent('''
                    &nbsp;
                    
                    **Luis Llera García**: 
                        
                    Graduated in Economics by [University Rey Juan Carlos](https://www.urjc.es/)
                    
                    *2013-2018*
                    
                    Master in Data Science for Finance by [CUNEF](https://www.cunef.edu/web/master-en-data-science-para-finanzas)
                    
                    *2018-2019*
                    
                    Programming Languages:
                        
                    * R (including Shiny)
                    * Python (including Dash)
                    * SQL
                    * Pyspark
                    
                    &nbsp;
                    [Github](https://github.com/Lleraoner)
                     
                    &nbsp;
                    
                    &nbsp;
                    
                    '''))
                ], style={'width': '33%', 'display': 'inline-block'}),
            ]),
    ])
], style={'textAlign': 'center'})
    
    
    

@app.callback(
    Output(component_id='overall', component_property='children'),
    [Input(component_id='dropdown', component_property='value')]
)

def update_output_div(input_value):
    overall = df[df.Name == input_value].Overall.iloc[0]
    return '{} overall: {}'.format(input_value, overall)

@app.callback(
    Output(component_id='radar-chart', component_property='src'),
    [Input(component_id='dropdown', component_property='value')]
)

def update_radar_chart(input_value):    
    out_url = fig_to_uri(generate_radar_chart(input_value))
    return out_url

@app.callback(
    Output(component_id='wage', component_property='children'),
    [Input(component_id='dropdown', component_property='value')]
)

def update_output_div2(input_value):
    wage = df[df.Name == input_value].Wage.iloc[0]
    return '{} wage: {}'.format(input_value, wage)


@app.callback(
    Output(component_id='position', component_property='children'),
    [Input(component_id='dropdown', component_property='value')]
)
def update_output_div3(input_value):
    ov = df[df.Name == input_value].Overall.iloc[0]
    wa = df[df.Name == input_value].Wage.iloc[0]
    dataClass = clf.predict([[ov,wa]])
    posicion = 0

    for i in range(1, 27):
        if dataClass == i:
            posicion = misVar[i]
            
    return '{} field position: {}'.format(input_value, posicion)

@app.callback(
    Output(component_id='relcla', component_property='children'),
    [Input(component_id='dropdown', component_property='value')]
)
def update_output_div4(input_value):
    relcla = df[df.Name == input_value]['Release Clause'].iloc[0]
    return '{} release clause: {}'.format(input_value, relcla)
#@app.callback(
    #dash.dependencies.Output('table-container', 'children'),
    #[dash.dependencies.Input('dropdown', 'value')])
#def display_table(dropdown_value):
   # if dropdown_value is None:
       # return gen_good_table(df[0:10])

    #dff = df[df.state.str.contains('|'.join(dropdown_value))]
    #return gen_good_table(dff[0:10])

@app.callback(
    dash.dependencies.Output('table_predict', 'children'),
    [dash.dependencies.Input('dropdown', 'value')])
def display_table_predict(dropdown_value):
        return generate_predict_dataframe(dropdown_value)

if __name__ == '__main__':
    app.run_server(debug=True)