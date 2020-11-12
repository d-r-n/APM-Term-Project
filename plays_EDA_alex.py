# Play Analysis
import pandas as pd
import seaborn as sns
from matplotlib import pyplot as plt

# Read in data, getting rid of na pass results and pass results that are 'R'
plays = pd.read_csv('./nfl-big-data-bowl-2021/plays.csv')
plays = plays[plays['passResult'].notna()]
plays = plays[plays['passResult'] != 'R']

p_head = plays.head()

#Creating and adding dummy variables for the pass Results
dummies = pd.get_dummies(plays['passResult'])

plays = plays.join(dummies)
############################################################################
#Grouping by gameId and possessionTeam
playid_possessionTeam = plays.groupby(by=['gameId', 'possessionTeam'],
                                      as_index=False)['C', 'I', 'IN', 'S']\
                                      .agg('sum')
                    
# Used to compute percentage complete
p_p_total = pd.DataFrame(plays.groupby(by=['gameId', 'possessionTeam'])\
                 ['passResult'].agg('size')).reset_index()

#joining
playid_possessionTeam = playid_possessionTeam.merge(p_p_total, on=['gameId', 'possessionTeam'])

#Computing percentage complete
playid_possessionTeam['C_pct'] = playid_possessionTeam['C']/playid_possessionTeam['passResult']

print(playid_possessionTeam['possessionTeam'].value_counts())

#average passing success per team
team_pass_avg = playid_possessionTeam.groupby(by='possessionTeam')['C_pct'].agg('mean').reset_index()
team_pass_avg = team_pass_avg.sort_values(by='C_pct', ascending=False)

fig, ax = plt.subplots(figsize=(12,8))
sns.color_palette(palette='dark', n_colors=len(team_pass_avg))
bar = sns.barplot(x='possessionTeam', y='C_pct', data=team_pass_avg)
bar.set_xticklabels(team_pass_avg['possessionTeam'],rotation=30)

############################################################################

# LOOK INTO HOW TO GRAPH/GROUP THESE. NOT SURE THIS IS QUITE THE RIGHT TRACK

#grouping by team and formation, getting sum of each pass outcome
team_oform = plays.groupby(by=['possessionTeam', 'offenseFormation'],
                           as_index=False)['C', 'I', 'IN', 'S'].agg('sum')

# grouping by team and formation, getting total number of passes
t_o_total = pd.DataFrame(plays.groupby(by=['possessionTeam', 'offenseFormation'])\
                         ['passResult'].agg('size')).reset_index()

#merging
team_oform = team_oform.merge(t_o_total)

#calculating % complete
team_oform['C_pct'] = team_oform['C']/team_oform['passResult']

#Finding the max completion percentage by team
best_form = pd.DataFrame(team_oform.groupby(by=['possessionTeam'])\
                         ['C_pct'].agg('max')).reset_index()

#rename, and merging former DF so know which formations resulted in that max % complete
best_form.rename({'max':'C_pct'}, axis='columns', inplace=True)
best_form = best_form.merge(team_oform, how='left')

best_form.drop(['I', 'IN', 'S'], axis=1, inplace=True)

#Dropping rows that have only 1 complete pass
mask = (best_form['C'] > 1)

best_form = best_form[mask]

############################################################################