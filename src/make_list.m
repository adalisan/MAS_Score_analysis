function make_list(permute)

    rand('state',sum(100.*clock));
    %opening MCI classification document and list of ids for region (QC and
    %non_QC

    fid1=fopen('W1_MCI classifications_NORMS_based_ESBs only_for Anne.csv','r');
    datatypes=textscan(fid1,'%4s%s%s%f%f%f%*[^\n]','Delimiter',',','HeaderLines',1);
    cell_ids=datatypes{1};
    ids=cellfun(@str2num,cell_ids);
    types=datatypes{3};
    genders=datatypes{6};

    fid2=fopen(strcat('scripts/datalist_all.txt'),'r');
    datalist1=textscan(fid2,'%4s%*[^\n]');
    cell_mri_ids1=datalist1{1};
    mri_ids=cellfun(@str2num,cell_mri_ids1);

 
    %initializing vectors for classification
    normal_ids_m=[];
    normal_ids_f=[];
    MCI_ids_amnestic_m=[];
    MCI_ids_amnestic_f=[];
    MCI_ids_nonamnestic_m=[];
    MCI_ids_nonamnestic_f=[];
    MCI_ids_other=[];
    unclassified_ids=[];
    not_in_list=[];

    %classifying each id
    for n=1:size(mri_ids,1)
        id=mri_ids(n);
        index=find(ids==id);
        gender=genders(index);
        if size(index,1)==0;
            not_in_list=[not_in_list;id];
        else type=types{index};
            type;
            if strcmp(type,'normal') | ...
               strcmp(type,'normal but some missing data') | ...
               strcmp(type,'normal + MFI elevated IADLs')
                if gender==1
                    normal_ids_m=[normal_ids_m;id];
                elseif gender==2
                    normal_ids_f=[normal_ids_f;id];
                end
            elseif strcmp(type,'amdMCI') | ... 
                   strcmp(type,'amnestic MCI unclassified') | ...
                   strcmp(type,'aMCI')
               if gender==1
                   MCI_ids_amnestic_m=[MCI_ids_amnestic_m;id];
               elseif gender==2
                   MCI_ids_amnestic_f=[MCI_ids_amnestic_f;id];
               end
            elseif strcmp(type,'nmdMCI') | ... 
                   strcmp(type,'nMCI')
                if gender==1
                    MCI_ids_nonamnestic_m=[MCI_ids_nonamnestic_m;id];
                elseif gender==2
                    MCI_ids_nonamnestic_f=[MCI_ids_nonamnestic_f;id];
                end
            elseif strcmp(type, 'nonamnestic MCI unclassified') | ...
                   strcmp(type,'MCI but no complaints') | ...
                   strcmp(type, 'MCI + MFI elevated IADLs')            
                    MCI_ids_other=[MCI_ids_other;id];
            else
                unclassified_ids=[unclassified_ids;id];
            end
        end
    end

    %writing classifications to file
    fid4=fopen(strcat('id_files/wave1_normal.txt'),'w');
    fid4m=fopen(strcat('id_files/wave1_normal_m.txt'),'w');
    fid4f=fopen(strcat('id_files/wave1_normal_f.txt'),'w');
    
    fid5=fopen(strcat('id_files/wave1_amnesticMCI.txt'),'w');
    fid5m=fopen(strcat('id_files/wave1_amnesticMCI_m.txt'),'w');
    fid5f=fopen(strcat('id_files/wave1_amnesticMCI_f.txt'),'w');
    
    fid6=fopen(strcat('id_files/wave1_nonamnesticMCI.txt'),'w');
    fid6m=fopen(strcat('id_files/wave1_nonamnesticMCI_m.txt'),'w');
    fid6f=fopen(strcat('id_files/wave1_nonamnesticMCI_f.txt'),'w');
    
    fid7=fopen(strcat('id_files/wave1_otherMCI.txt'),'w');
    
    fprintf(fid4,'%04u\n',[normal_ids_m;normal_ids_f]);
    fprintf(fid4m,'%04u\n',normal_ids_m);
    fprintf(fid4f,'%04u\n',normal_ids_f);
    
    fprintf(fid5,'%04u\n',[MCI_ids_amnestic_m;MCI_ids_amnestic_f]);
    fprintf(fid5m,'%04u\n',MCI_ids_amnestic_m);
    fprintf(fid5f,'%04u\n',MCI_ids_amnestic_f);
    
    fprintf(fid6,'%04u\n',[MCI_ids_nonamnestic_m;MCI_ids_nonamnestic_f]);
    fprintf(fid6m,'%04u\n',MCI_ids_nonamnestic_m);
    fprintf(fid6f,'%04u\n',MCI_ids_nonamnestic_f);
    
    fprintf(fid7,'%04u\n',MCI_ids_other);
    
    %generating a permutated sample if needed
    if permute
        sprintf('permuting...')
        ids=[normal_ids;MCI_ids_amnestic];
        ids_mixed=randsample(ids,length(ids));
        fid8=fopen(strcat('wave1_',region,'_normtest1_aMCI.txt'),'w');
        fprintf(fid8,'%04u\n',ids_mixed(1:length(MCI_ids_amnestic)));
        fid9=fopen(strcat('wave1_',region,'_normtest2_aMCI.txt'),'w');
        fprintf(fid9,'%04u\n',ids_mixed(length(MCI_ids_amnestic)+1:length(ids_mixed)));
    end    
    %normal_ids and MCI_ids_amnestic
end
