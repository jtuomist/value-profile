import json
import csv
import requests
from bs4 import BeautifulSoup
from pathlib import Path
import time

# Configuration
CANDIDATE_URL = "https://vaalikone.fi/kunta2025/hs/tulokset/ehdokkaat/"

PARTY_MAPPING = {
    '231dead0-c733-4991-a8c0-0d511e11cdc4': 'Vihreät',
    '9eedf280-8da6-4d74-9ee4-7f8d3b87cd90': 'Keskusta',
    'aaa152ae-1854-48ef-986e-ea764ed56a24': 'SDP',
    '8c1e3917-0920-4893-9597-1e8860371f1c': 'Perussuomalaiset',
    '2ef3060b-602f-43a9-8008-ae8a0c427426': 'Kokoomus',
    '72b23234-d423-4bfa-a463-1f6649df0e0a': 'Kristillisdemokraatit',
    'c2fa2f51-0d85-46d0-ab42-89593f70d92b': 'Vasemmistoliitto',
    'afc6334d-cc8c-4ddf-a6f6-949e11aad7e6': 'Vapauden liitto',
    'f4bbeb67-3b0a-4b81-9606-0f192652e91b': 'RKP',
    'b782e02f-0fdf-42e5-be9c-53cdd89615c4': 'Liike Nyt',
    '': 'Tieto puuttuu',
}

def extract_from_html_file(file_path):
    """Extract data from a local HTML file (original approach)"""
    
    # Get first and last name from filename
    filename = Path(file_path).stem
    words = filename.split()
    
    # Assume filename format: "... first_name last_name"
    if len(words) >= 2:
        filename_first_name = words[-2]
        filename_last_name = words[-1]
    else:
        filename_first_name = filename
        filename_last_name = ""
    
    # Read and parse HTML
    with open(file_path, 'r', encoding='utf-8') as file:
        content = file.read()
    
    return parse_html_content(content, filename_first_name, filename_last_name, "", str(file_path))

def extract_from_uuid_file(uuid_file_path):
    """Extract data by reading UUIDs and fetching from web"""
    
    all_data = []
    
    # Read UUIDs from file
    with open(uuid_file_path, 'r', encoding='utf-8') as file:
        uuids = [line.strip() for line in file if line.strip() and not line.startswith("#")]
    
    print(f"Found {len(uuids)} UUIDs in {uuid_file_path}")
    
    for i, uuid in enumerate(uuids, 1):
        print(f"Processing UUID {i}/{len(uuids)}: {uuid}")
        
        # Fetch HTML content from web
        url = CANDIDATE_URL + uuid
        try:
            response = requests.get(url, timeout=30)
            response.raise_for_status()
            content = response.text
            
            # Parse the content - names will be extracted from JSON data
            data = parse_html_content(content, "", "", uuid, url)
            if data:
                all_data.extend(data)
                print(f"  Successfully extracted data for {data[0].get('first_name', '')} {data[0].get('last_name', '')}")
            else:
                print(f"  Failed to extract data from {url}")
                
        except requests.RequestException as e:
            print(f"  Error fetching {url}: {e}")
            continue
        except Exception as e:
            print(f"  Error processing {url}: {e}")
            continue
        
        # Be nice to the server
        time.sleep(1)
    
    return all_data

def parse_html_content(html_content, filename_first_name, filename_last_name, uuid, source_info):
    """Parse HTML content and extract data (common logic for both approaches)"""
    
    soup = BeautifulSoup(html_content, 'html.parser')
    
    # Find the script tag with JSON data
    script_tag = soup.find('script', {'id': '__NEXT_DATA__'})
    if not script_tag:
        print(f"No JSON data found in {source_info}")
        return None
    
    # Parse JSON data
    try:
        json_data = json.loads(script_tag.string)
        page_props = json_data['props']['pageProps']
        answers_data = page_props['answers']
        candidate_data = page_props.get('candidate', {})
    except (json.JSONDecodeError, KeyError) as e:
        print(f"Error parsing JSON in {source_info}: {e}")
        return None
    
    # Extract candidate information
    # Use JSON data for names if available, otherwise use filename
    json_first_name = candidate_data.get('firstName', '')
    json_last_name = candidate_data.get('lastName', '')
    
    candidate_info = {
        'first_name': json_first_name or filename_first_name,
        'last_name': json_last_name or filename_last_name,
        'nomination_area': candidate_data.get('nominationArea', ''),
        'occupation': candidate_data.get('occupation', ''),
        'candidate_number': candidate_data.get('candidateNumber', ''),
        'age': candidate_data.get('age', ''),
        'party': PARTY_MAPPING[candidate_data.get('party', '')],
        'municipality': candidate_data.get('municipality', ''),
        'uuid': uuid,  # Empty for HTML files, populated for web fetching
    }
    
    # Extract political values (flatten the nested values object)
    values = candidate_data.get('values', {})
    political_values = {}
    for dimension, score in values.items():
        # Clean up dimension names for CSV columns
        clean_dimension = dimension.replace(' - ', '_').replace(' ', '_').replace('ä', 'a').replace('ö', 'o')
        political_values[f'value_{clean_dimension}'] = score
    
    # Extract questions and answers
    extracted_data = []
    
    for theme_data in answers_data:
        theme_name = theme_data['theme']['name']
        
        for question in theme_data['questions']:
            question_text = question['text']
            answer = question.get('answer', '')  # Handle missing answers
            explanation = question.get('explanation', '')
            question_id = question.get('questionId', question.get('id', ''))
            
            # Combine all data for this row
            row_data = {
                'theme': theme_name,
                'question_id': question_id,
                'question_text': question_text,
                'answer': answer,
                'explanation': explanation,
                **candidate_info,  # Add all candidate info
                **political_values  # Add all political values
            }
            
            extracted_data.append(row_data)
    
    return extracted_data

def process_all_files(directory_path, output_csv_path):
    """Process all HTML files in directory and create CSV (original approach)"""
    
    all_data = []
    html_files = []
    
    # Find all HTML files
    for file_path in Path(directory_path).glob("*.html"):
        html_files.append(file_path)
    
    if not html_files:
        print(f"No HTML files found in {directory_path}")
        return
    
    print(f"Found {len(html_files)} HTML files")
    
    # Process each file
    for file_path in html_files:
        print(f"Processing: {file_path.name}")
        data = extract_from_html_file(file_path)
        
        if data:
            all_data.extend(data)
            print(f"  Extracted {len(data)} questions for {data[0]['first_name']} {data[0]['last_name']}")
        else:
            print("  Failed to extract data")
    
    write_csv_data(all_data, output_csv_path)

def process_uuid_file(uuid_file_path, output_csv_path):
    """Process UUIDs from file and fetch data from web"""
    
    print(f"Processing UUIDs from: {uuid_file_path}")
    all_data = extract_from_uuid_file(uuid_file_path)
    
    if all_data:
        write_csv_data(all_data, output_csv_path)
    else:
        print("No data extracted from UUID file")

def write_csv_data(all_data, output_csv_path):
    """Write data to CSV file"""
    
    if not all_data:
        print("No data to write")
        return
    
    # Get all possible fieldnames from all records
    fieldnames = set()
    for row in all_data:
        fieldnames.update(row.keys())
    
    # Order fieldnames logically
    ordered_fields = [
        'first_name', 'last_name', 'uuid', 'nomination_area', 'occupation', 
        'candidate_number', 'age', 'party', 'municipality', 'theme', 
        'question_id', 'question_text', 'answer', 'explanation'
    ]
    
    # Add political values fields
    value_fields = [f for f in fieldnames if f.startswith('value_')]
    value_fields.sort()
    
    # Only include fields that actually exist in the data
    final_fieldnames = [f for f in ordered_fields if f in fieldnames] + value_fields
    
    with open(output_csv_path, 'w', newline='', encoding='utf-8') as csvfile:
        writer = csv.DictWriter(csvfile, fieldnames=final_fieldnames)
        writer.writeheader()
        writer.writerows(all_data)
    
    print(f"\nCSV file created: {output_csv_path}")
    print(f"Total records: {len(all_data)}")
    print(f"Unique respondents: {len(set((row['first_name'], row['last_name']) for row in all_data))}")

def auto_detect_input_type(input_path):
    """Automatically detect if input is HTML files directory or UUID file"""
    
    path = Path(input_path)
    
    if path.is_dir():
        # Check if directory contains HTML files
        html_files = list(path.glob("*.html"))
        if html_files:
            return "html_directory"
    
    elif path.is_file():
        if path.suffix.lower() in ['.txt', '.csv']:
            return "uuid_file"
        elif path.suffix.lower() in ['.html', '.htm']:
            return "single_html"
    
    return "unknown"

# Main execution
if __name__ == "__main__":
    import sys
    
    # Configuration
    HTML_DIRECTORY = "."  # Current directory
    UUID_FILE = "candidate_uuids.txt"
    OUTPUT_CSV = "survey_responses_full.csv"
    
    # Check command line arguments
    if len(sys.argv) > 1:
        input_path = sys.argv[1]
    else:
        input_path = HTML_DIRECTORY
    
    # Auto-detect input type
    input_type = auto_detect_input_type(input_path)
    
    print(f"Input: {input_path}")
    print(f"Detected type: {input_type}")
    
    if input_type == "html_directory":
        print("Processing HTML files from directory...")
        process_all_files(input_path, OUTPUT_CSV)

    elif input_type == "uuid_file":
        print("Processing UUIDs from file and fetching from web...")
        process_uuid_file(input_path, OUTPUT_CSV)
        
    elif input_type == "single_html":
        print("Processing single HTML file...")
        data = extract_from_html_file(input_path)
        if data:
            write_csv_data(data, OUTPUT_CSV)
            
    elif input_type == "unknown":
        print("Could not detect input type. Please specify:")
        print("1. Directory containing HTML files")
        print("2. Text file containing UUIDs (one per line)")
        print("3. Single HTML file")

    else:
        print("Unknown input type")
    
    print("\nProcessing complete!")
