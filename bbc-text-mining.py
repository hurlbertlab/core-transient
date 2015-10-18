"""Extract text data from Breeding Bird Census pdfs"""

import os
import re
import string
from glob import glob

import pandas as pd

def convert_pdf_to_images(filename):
    """Convert a pdf to images"""
    filename = os.path.splitext(filename)[0]
    os.system("convert -density 350 -crop 0x0+0+315 {0}.pdf {0}.png".format(filename))

def ocr(filename):
    """OCR a file using tesseract"""
    filename = os.path.splitext(filename)[0]
    os.system("tesseract {0}.png {0}".format(filename))

def convert_pdfs_to_text(path):
    """Convert all PDFs in a directory to text

    Use convert to convert to images and tesseract for OCR
    
    """
    pdfs = glob(os.path.join(path, "*.pdf"))
    for pdf in pdfs:
        convert_pdf_to_images(pdf)
        
        #multi-page pdfs create multiple png files so loop over them
        pngs = glob(os.path.join(path, "*.png"))
        for png in pngs:
            ocr(png)

def cleanup_nonpara_pages(path, para_starts):
    """Remove text and png files for pages that aren't the core paragraph data"""
    for year in para_starts:
        pages  = range(para_starts[year] - 1) #pages are not zero indexed
        for page in pages:
            os.remove(os.path.join(path, "BBC{}-{}.txt".format(year, page)))
            os.remove(os.path.join(path, "BBC{}-{}.png".format(year, page)))

def combine_txt_files_by_yr(path, years):
    """Combine multiple text files into a single file for each year

    File names have the general format: BBC1988-0.txt
    
    """
    for year in years:
        with open(os.path.join(path, "bbc_combined_{}.txt".format(year)), 'w') as outfile:
            filenames = glob(os.path.join(path, "BBC{}*.txt".format(year)))
            sorted_filenames = sorted_nicely(filenames)
            for fname in sorted_filenames:
                with open(fname) as infile:
                    outfile.write(infile.read())

def sorted_nicely(l): 
    """ Sort the given iterable in the way that humans expect.

    From:
    http://stackoverflow.com/questions/2669059/how-to-sort-alpha-numeric-set-in-python
    
    """ 
    convert = lambda text: int(text) if text.isdigit() else text 
    alphanum_key = lambda key: [ convert(c) for c in re.split('([0-9]+)', key) ] 
    return sorted(l, key = alphanum_key)

def get_site(inputstring):
    """Check if line is location data and if so return location"""
    site_re = "^([0-9]{1,2})\. ([A-Z —-]{2,})"
    site_search = re.search(site_re, inputstring)
    if site_search:
        return (site_search.group(1), site_search.group(2))

def is_start_main_block(inputstring):
    """Check if line is the first line of the main block of data"""
    return inputstring.startswith("Location: ")

def parse_block(block, site_name, site_num):
    """Parse a main data block from a BBC file"""
    # Cleanup difficult issues manually
    # Combination of difficult \n's and OCR mistakes
    replacements = {'Cemus': 'Census',
                    'Description\nof Plot': 'Description of Plot',
                    'De-\nscription of Plot': 'Description of Plot',
                    'Description of\nPlot': 'Description of Plot',
                    'Descrip-\ntion of Plot': 'Description of Plot',
                    'Solitary Vireo, 1,0;': 'Solitary Vireo, 1.0;',
                    'Common Yellowthroat, 14,0': 'Common Yellowthroat, 14.0',
                    'Bobolink; 9.0 territories': 'Bobolink, 9.0 territories'}
    for replace in replacements:
        block = block.replace(replace, replacements[replace])
    p = re.compile(r'((?:Location|Continuity|Size|Description of Plot|Edge|Topography and Elevation|Weather|Coverage|Census|Total|Visitors|Remarks|Other Observers|Acknowledgments)):') # 'Cemus' included as a mis-OCR of Census
    split_block = p.split(block)[1:] #discard first value; an empty string
    block_dict = {split_block[i]: split_block[i+1] for i in range(0, len(split_block), 2)}
    block_dict['SiteName'] = site_name
    block_dict['SiteNumInCensus'] = site_num
    return block_dict

def parse_txt_file(infile):
    """Parse a BBC text file"""
    first_site = True
    recording = False
    data = dict()
    for line in infile:
        site_info = get_site(line)
        if site_info:
            print(site_info)
            if not first_site:
                data[site_num] = parse_block(main_block, site_name, site_num)
            first_site = False
            site_num, site_name = site_info
            site_num = int(site_num)
            recording = False
        elif is_start_main_block(line):
            main_block = ''
            recording = True
        if recording:
            if line.strip():
                main_block += line
    return(data)

def get_latlong(location):
    """Extract the latitude and longitude from the Location data"""
    regex = "([0-9]{1,3})°([0-9]{1,2})[ ]*[’|'|‘]N,[ |\\n]([0-9]{2,3})°([0-9]{1,2})[’|'|‘]W"
    search = re.search(regex, location)
    if search:
        lat_deg, lat_min = int(search.group(1)), int(search.group(2))
        long_deg, long_min = int(search.group(3)), int(search.group(4))
        lat_decdeg = lat_deg + lat_min / 60.0
        long_decdeg = long_deg + long_min / 60.0
        return (lat_decdeg, long_decdeg)

def extract_counts(data, site, year):
    """Split the Census text block into species and counts"""
    census_data = data['Census']
    census_data = re.sub(r'\([^)]+\)', '', census_data) # remove parentheticals (which include ;)
    census_data = census_data.replace('territories', '')
    census_data = census_data.split(';')
    counts_data = pd.DataFrame(columns = ['site', 'year', 'species', 'count', 'status'])
    for record in census_data:
        if record.strip(): # Avoid occasional blank lines
            species, count = record.split(',')
            species = get_cleaned_species(species)
            counts_record = pd.DataFrame({'year': year,
                                          'site': site,
                                          'species': [species],
                                          'count': [count.strip(' .\n')],
                                          'status': ['resident']})
            counts_data = counts_data.append(counts_record, ignore_index = True)

    if 'Visitors' in data:
        visitor_data = data['Visitors'].split(',')
        for species in visitor_data:
            species = get_cleaned_species(species)
            counts_record = pd.DataFrame({'species': [species],
                                          'count': [None],
                                          'status': ['visitor']})
            counts_data = counts_data.append(counts_record, ignore_index = True)
    
    return counts_data

def get_clean_size(size_data):
    """Remove units and whitespace"""
    return float(size_data.strip(' ha.\n'))

def get_cleaned_species(species):
    """Cleanup species names"""
    species = species.strip().replace('-\n', '-')
    species = species.replace('\n', ' ')
    species = species.strip()
    return species


def get_cleaned_string(string_data):
    """Do basic cleanup on string data

    1. Remove \n's
    2. Strip whitespace

    """

    string_data = string_data.strip().replace('-\n', '')
    string_data = string_data.replace('\n', ' ')
    string_data = string_data.strip()
    return string_data

def clean_string_fields(site_data):
    """Do basic cleanup on simple string fields for a site"""
    string_fields = ['Description of Plot', 'Edge', 'Location', 'Remarks', 'SiteName']
    for field in string_fields:
        if field in site_data:
            site_data[field] = get_cleaned_string(site_data[field])
    return site_data

def extract_coverage(coverage):
    """Extract number of hours and number of visits from Coverage"""
    coverage = get_cleaned_string(coverage)
    extracted = dict()
    re_with_times = '([0-9]{1,3}\.{0,1}[0-9]{0,2}) h; ([0-9]{1,2}) [V|v]isits \(([^)]+)\);(.*)'
    re_no_times = '([0-9]{1,3}\.{0,1}[0-9]{0,2}) h; ([0-9]{1,2}) [V|v]isits;(.*)'
    search = re.search(re_with_times, coverage)
    if search:
        extracted['hours'] = float(search.group(1))
        extracted['visits'] = int(search.group(2))
        extracted['times'] = search.group(3)
        extracted['notes'] = search.group(4)
    else:
        search = re.search(re_no_times, coverage)
        extracted['hours'] = float(search.group(1))
        extracted['visits'] = int(search.group(2))
        extracted['notes'] = search.group(3)
    return extracted

def extract_total(total):
    """Extract the total number of species and total territories"""
    total = get_cleaned_string(total)
    extracted = dict()
    regex = '([0-9]{1,3}) species; ([0-9]{1,4}\.{0,1}[0-9]{0,1}) territories \(([^)]+)\).'
    search = re.search(regex, total)
    extracted['total_species'] = int(search.group(1))
    extracted['total_territories'] = float(search.group(2))
    extracted['total_terr_notes'] = search.group(3)
    return extracted

para_starts = {1988: 4, 1989: 6, 1990: 6, 1991: 6,
               1992: 7, 1993: 7, 1994: 7, 1995: 6}
data_path = "./data/raw_datasets/BBC_pdfs/"
#convert_pdfs_to_text(data_path)
#cleanup_nonpara_pages(data_path, para_starts)
#combine_txt_files_by_yr(data_path, para_starts.keys())

counts_table = pd.DataFrame(columns = ['site', 'year', 'species', 'count', 'status'])
years = [1990,]

for year in years:
    datafile = os.path.join(data_path, "bbc_combined_{}.txt".format(year))
    with open(datafile) as infile:
        data = parse_txt_file(infile)
        for site in data:
            print(site)
            data[site]['latitude'], data[site]['longitude'] = get_latlong(data[site]['Location'])
            counts_table = counts_table.append(extract_counts(data[site], site, year))
            data[site]['Size'] = get_clean_size(data[site]['Size'])
            data[site]['Coverage'] = extract_coverage(data[site]['Coverage'])
            data[site]['Total'] = extract_total(data[site]['Total'])
            data[site] = clean_string_fields(data[site])
