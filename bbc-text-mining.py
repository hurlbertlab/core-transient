"""Extract text data from Breeding Bird Census pdfs"""

import os
import re
import string
from glob import glob

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
    site_re = "^([1-9]{1,2})\. ([A-z ]+)"
    site_search = re.search(site_re, inputstring)
    if site_search:
        return (site_search.group(1), site_search.group(2))

def is_start_main_block(inputstring):
    """Check if line is the first line of the main block of data"""
    return inputstring.startswith("Location: ")

def parse_block(block, site_name):
    """Parse a main data block from a BBC file"""
    p = re.compile(r'((?:Location|Continuity|Size|Description of Plot|Edge|Topograph and Elevation|Weather|Coverage|Census|Total|Visitors|Remarks|Acknowledgments)):')
    split_block = p.split(block)[1:] #discard first value; an empty string
    block_dict = {split_block[i]: split_block[i+1] for i in range(0, len(split_block), 2)}
    block_dict['SiteName'] = site_name
    return block_dict

def parse_txt_file(infile):
    """Parse a BBC text file"""
    first_site = True
    recording = False
    data = dict()
    for line in infile:
        site_info = get_site(line)
        if site_info:
            if not first_site:
                data[site_num] = parse_block(main_block, site_name)
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

para_starts = {1988: 4, 1989: 6, 1990: 6, 1991: 6,
               1992: 7, 1993: 7, 1994: 7, 1995: 6}
data_path = "./data/raw_datasets/BBC_pdfs/"
convert_pdfs_to_text(data_path)
cleanup_nonpara_pages(data_path, para_starts)
combine_txt_files_by_yr(data_path, para_starts.keys())
with open(os.path.join(data_path, "bbc_combined_1990.txt")) as infile:
    data = parse_txt_file(infile)
