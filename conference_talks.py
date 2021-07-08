import pandas as pd
import requests
from bs4 import BeautifulSoup
import re
import pandas as pd
import pdb
import time
from random import randint

years = range(1971,2022)
months = ['04', '10']
home_page = 'https://www.churchofjesuschrist.org'

var_names = ['year', 'month', 'speaker', 'title', 'text']
conf_data = []

for year in years:
    for month in months:
        start_time_month = time.time()
        
        # Download conference html file for given date
        conf_page = requests.get(f'{home_page}/general-conference/{year}/{month}?lang=eng')
        conf_soup = BeautifulSoup(conf_page.content, 'html.parser')
        
        # Information for each talk in given conference
        links = conf_soup.find_all(class_="item-3cCP7")
        for link_contents in links:
            try:
                speaker_name = link_contents.find(class_='subtitle-MuO4X')
                if speaker_name is None:
                    continue
                speaker_name = speaker_name.string
                subdir = link_contents.get('href')
                
                # Skip talk if there is no html site with talk text (usually general women's meeting)
                if re.search('media', subdir) != None:
                    print(f'Missing talk text: {speaker_name}, {month} {year}')
                    conf_data.append((year, month, speaker_name, None, None))
                    continue
            except:
                print('Exception: Problem getting speaker name')
                print(f'{month} {year} Conference')
                print(link_contents)
                raise
                
            title_obj = link_contents.find('div', class_="itemTitle-23vMm")
            title = title_obj.find('p').getText()
#             print(f"\nCurrent Talk: {title}")
#             print(f"Date: {month}, {year}")
#             print(f"Speaker: {speaker_name}")
            
            # Refreshing page up to 3 times if an error occurs
            talk_loaded = False
            iterations = 0
            MAX_ITER = 3
            while talk_loaded == False and iterations < MAX_ITER:
                # Get talk text
                talk_page = requests.get(f'{home_page}{subdir}')
                talk_soup = BeautifulSoup(talk_page.content, 'html.parser')
                talk_title = talk_soup.title.string
                
                # Check that GET request was successful
                if iterations == MAX_ITER:
                    print(f'Talk failed loading {MAX_ITER} times\nAborting')
                    raise
                elif talk_title == 'Service Not Available':
                    print(f'WARNING: {month} {year} conference talk by {speaker_name} did not load--{talk_page}')
                    iterations += 1
                    time.sleep(randint(2,5))
                else:
                    talk_loaded = True
            
            # Replace lettered references with reference content
            references = talk_soup.find_all(id=re.compile('note[0-9]+'))
            
            # Combine references that are from same note
            new_references = []
            note = ''
            for ref in references:
                # If this is the beginning of a section of a new footnote, append as new footnote
                new_string = ' '.join(ref.stripped_strings)
                next_note = re.search('note[0-9]+', ref.get('id')).group(0)
                # If the next section is part of the same footnote, add to same footnote
                if note == next_note:
                    new_references[-1] = ' '.join([new_references[-1], new_string])
                else:
                    new_references.append(new_string)
                note = next_note
            
            references = new_references
            
            # Replace number in talk body text with formatted citation, be it scripture or other reference
            talk_refs = talk_soup.find_all(href=re.compile('note[0-9]+'))
            for i, new_string in enumerate(references):
                try:
                    talk_refs[i].string.replace_with(f' ({new_string}) ')
                except:
                    print('Exception: Problem replacing reference string')
                    print(f'{month} {year} Conference')
                    print(f'Speaker: {speaker_name}, Talk: {talk_title}')
                    print(f'Talk total references: {len(references)}, Currently on: {i}')
                    raise
            
            # Get talk body text after adding references
            try:
                talk_text = talk_soup.find(class_='body-block').get_text(separator=' ', strip=True)
            except:
                print('Exception: Problem obtaining body text')
                print(f'{month} {year} Conference')
                print(f'Speaker: {speaker_name}, Talk: {talk_title}')
                raise
            conf_data.append((year, month, speaker_name, talk_title, talk_text))
        
        # Compute and report data scraping time for each conference session
        time_min_month = round( (time.time()-start_time_month)/60, 2)
        print(f'Extracted {month} {year} conference talks in {time_min_month} minutes\n')
            
conf_df = pd.DataFrame(conf_data, columns=var_names)
conf_df.to_csv('conference.csv', index=False)
