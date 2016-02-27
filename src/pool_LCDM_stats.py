#! /usr/bin/env python
__author__ = 'Sancar'
import os
import os.path.join as join_p
import glob




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

hemis=['rh','lh']

wave1_dir1 = '/cis/project/sydney/data_fslcdm/MAS.20100921_Antsy/'
wave1_dir2 = "/cis/project/sydney/data_fslcdm/MAS.20110324_Antsy/'
for region in regions:
    region_name = hemi+'_'+region
    for hemi in hemis:
        for diag in diags:
            idfile='ids_5_subclass/Diag_'+diag+'_ID_list.csv' #wave1_${diag}_used.txt'
            id_fh = open(idfile,'r')
            ids = id_fh.readlines()
            for id in ids:

                for sub in subregions[region]:
                    sub_region=hemi+'_'+sub
                    output_fname = '/cis/home/sadali/pooled_analysis/pooled_5subclass_antsy_files_used/'+sub_region+'_'+diag+'_wave1_pooled_antsy.txt'
                    #of_handle = open (output_fname,'a')

                    for id in ids:
                        id = id.strip()
                    #   full_dir_names = os.listdir(wave1_dir1)
                    #   full_path =  os.path.join(wave1_dir1, x for x in full_dir_names if x.startswith(id), 'FSLCDM_v2.3',region_name,sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')
                        full_path =  os.path.join(wave1_dir1, id+"*", 'FSLCDM_v2.3',region_name,sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')
                        f = glob.glob(full_path):

                        if os.path.exists(f) and os.isfile(f) and os.path.getsize(f)>0:
                            os.system("cat "+f+" >> "+output_fname)
                        else:
                            full_path =  os.path.join(wave1_dir2, id+"*", 'FSLCDM_v2.3', region_name, sub_region+'_1_manseg_quartile_pialmsk_AntsyGrey.txt')
                            f = glob.glob(full_path)
                            if os.path.exists(f) and os.isfile(f) and os.path.getsize(f)>0:
                                os.system("cat "+f+" >> "+output_fname)


                        print id+" added to pooled stats"
