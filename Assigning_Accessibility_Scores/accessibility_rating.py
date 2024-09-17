import pandas as pd

def calculate_accessibility_scores(df):
    last_mention = {}
    previous_mention_scores = []
    saliency_scores = []
    competition_scores = []
    overall_scores = []
    narrative_order = [1, 10, 5, 6, 2, 3, 4, 8, 9, 7]  

    for i, row in df.iterrows():
        subject = row['Subject']
        current_narrative = row['Narrative']
        current_ref = row['Referent_Name']
        current_reference = row['Reference']
        current_discourse = row['Discourse']
        ref_key = (subject, current_ref) 

        if ref_key in last_mention:
            last_mention_info = last_mention[ref_key]
            current_narrative_index = narrative_order.index(current_narrative)
            last_mention_narrative_index = narrative_order.index(last_mention_info['narrative'])

            if last_mention_info['index'] == i - 1 and last_mention_narrative_index == current_narrative_index:
                previous_mention_score = 3  
            elif last_mention_narrative_index == current_narrative_index:
                previous_mention_score = 2  
            elif (current_narrative_index > 0 and 
                  last_mention_narrative_index == current_narrative_index - 1):
                previous_mention_score = 1 
            else:
                previous_mention_score = 0  
        else:
            previous_mention_score = 0  

        recent_mentions = df.loc[max(0, i-4):i-1]
        recent_mentions = recent_mentions[(recent_mentions['Narrative'] == current_narrative) & (recent_mentions['Subject'] == subject)]
        mention_count = recent_mentions['Referent_Name'].tolist().count(current_ref)
        saliency_score = 2 if mention_count > 2 else 1 if mention_count >= 1 else 0

        if current_discourse == 'Maintenance':
            competition_score = 0  
        else:
            recent_references = df.loc[(df['Narrative'] == current_narrative) & (df['Subject'] == subject) & (df.index < i)]
            competing_referents = set(recent_references['Referent_Name']) - {current_ref}
            competing_entities = len(competing_referents)
            competition_score = -2 if competing_entities >= 2 else -1 if competing_entities == 1 else 0

        # Overall score
        overall_score = previous_mention_score + saliency_score + competition_score
        
        last_mention[ref_key] = {'index': i, 'narrative': current_narrative}
        previous_mention_scores.append(previous_mention_score)
        saliency_scores.append(saliency_score)
        competition_scores.append(competition_score)
        overall_scores.append(overall_score)
        
    df['Previous_Mention_Score'] = previous_mention_scores
    df['Saliency_Score'] = saliency_scores
    df['Competition_Score'] = competition_scores
    df['Accessibility'] = overall_scores
    return df

# Dataset
df = pd.read_csv('df.csv', sep=',')

# Drop unnecessary columns
df.drop(columns=['Accessibility'], inplace=True)

# Calculate the accessibility scores
df = calculate_accessibility_scores(df)

# Save the updated dataframe to a new CSV
df.to_csv('df.csv', index=False)

