#! /usr/bin/python
__author__ = 'Sancar'
import os
from  os.path import join as join_p
import glob

ROOT = "/cis/home/sadali/MAS_score_analysis/"


regions= ['cingulate','TL']

subregions={'TL':['stg','mtg','itg'], 'cingulate':[ 'postcing', 'antcing']}


diags=[
    'normal',
#    normal_m
#    normal_f
    'aMCI',
    'amdMCI',
#    amnesticMCI_m
#    amnesticMCI_f
    'nMCI',
    'nmdMCI'
#    nonamnesticMCI_m
#    nonamnesticMCI_f

#    normtest1_aMCI
#    normtest2_aMCI
]


hemis = ['rh','lh']


wave1_dir1 = '/cis/project/sydney/data_fslcdm/MAS.20100921_Antsy/'
wave1_dir2 = '/cis/project/sydney/data_fslcdm/MAS.20110324_Antsy/'
for region in regions:

    for hemi in hemis:
        region_name = hemi+'_'+region
        for diag in diags:
            idfile=join_p(ROOT,'data','ids_5_subclass','Diag_'+diag+'_ID_list.csv' ) #wave1_${diag}_used.txt'
            id_fh = open(idfile,'r')
            ids = id_fh.readlines()


            for sub in subregions[region]:
                sub_region=hemi+'_'+sub
                output_dir = join_p(ROOT,'data','pooled_analysis','pooled_5subclass_antsy_files_used')
                if not os.path.exists(output_dir):
                    os.makedirs(output_dir)
                output_fname = join_p(output_dir,sub_region+'_'+diag+'_wave1_pooled_antsy.txt')
                #of_handle = open (output_fname,'a')

                for id in ids:
                    id = id.strip()
                    full_dir_names = os.listdir(wave1_dir1)
                #   full_path =  os.path.join(wave1_dir1, x for x in full_dir_names if x.startswith(id), 'FSLCDM_v2.3',region_name,sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')
                    full_path = os.path.join(wave1_dir1, id+"*", 'FSLCDM_v2.3', region_name, sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')
                    print full_path
                    f = glob.glob(str(full_path))
                    #print "file matches for "+id

                    for file_match in f:
                      print file_match

                    if  len(f) > 0 and os.path.exists(f[0]) and os.path.isfile(f[0]) and os.path.getsize(f[0])>0:
                        os.system("cat "+f[0]+" >> "+output_fname)
                    else:

                        full_path =  os.path.join(wave1_dir2, id+"*", 'FSLCDM_v2.3', region_name, sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')

                        f = glob.glob(str(full_path))
                    #print "file matches for "+id

                        for file_match in f:
                          print file_match
                        if len(f) > 0  and os.path.exists(f[0]) and os.path.isfile(f[0]) and os.path.getsize(f[0])>0:
                            os.system("cat "+f[0]+" >> "+output_fname)



            print "pooled stats aggregated for "+ sub_region
