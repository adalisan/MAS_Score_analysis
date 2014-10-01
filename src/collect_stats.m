function collect_stats()

    rand('state',sum(100.*clock));
    %opening MCI classification document and list of ids for region (QC and
    %non_QC
    fid1=fopen('/cis/project/sydney/W1_MCI classifications_NORMS_based_ESBs only_for Anne.csv','r');

    datatypes=textscan(fid1,'%4s%s%s%f%f%f%f%s%s%s','Delimiter',',','HeaderLines',1);
    
    cell_ids=datatypes{1};
    ids=cellfun(@str2num,cell_ids);
    
    diags=datatypes{3};
    ages=datatypes{5};
    genders=datatypes{6};
    yrs_edu=datatypes{7};
    LearnEng=datatypes{8};

    fid2=fopen('/cis/project/sydney/id_files/wave1_good.txt','r');
    
    datalist1=textscan(fid2,'%4s%*[^\n]');
    cell_mri_ids1=datalist1{1};
    mri_ids=cellfun(@str2num,cell_mri_ids1);

 
  

    info(1)
    
    
    %initializing vectors for classificatio
    normal_ids={};
    MCI_ids_amnestic={};
    MCI_ids_nonamnestic={};
    MCI_ids_other={};
    unclassified_ids={};
    not_in_list={};

    %classifying each id
    first=1;
    for n=1:size(mri_ids,1)
        n
        id=mri_ids(n);
        index=find(ids==id);
        if size(index,1)==0;
            not_in_list=[not_in_list;id];
        else diag=diags{index};
            
            if strcmp(diag,'normal') | ...
               strcmp(diag,'normal but some missing data') | ...
               strcmp(diag,'normal + MFI elevated IADLs')
                diag=0;
            elseif strcmp(diag,'amdMCI') | ... 
                   strcmp(diag,'amnestic MCI unclassified') | ...
                   strcmp(diag,'aMCI')
               diag=1;
            elseif strcmp(diag,'nmdMCI') | ... 
                   strcmp(diag,'nMCI')
               diag=2;
            else
                continue
    %            elseif strcmp(diag, 'nonamnestic MCI unclassified') | ...
    %                   strcmp(diag,'MCI but no complaints') | ...
    %                   strcmp(diag, 'MCI + MFI elevated IADLs')            
    %                    MCI_ids_other={MCI_ids_other;cell_ids{index},ages{index},genders{index},yrs_edu{index},LearnEng{index}};
    %            else
    %                unclassified_ids={unclassified_ids;cell_ids{index},ages{index},genders{index},yrs_edu{index},LearnEng{index}};
    %       
            end

            if first==1
                id;
                diag;
                ages;
                info1=struct('Sub_id', {[id]}, 'Diag', {[diag]}, 'Age', {[ages(index)]}, ...
                    'Gender', {[genders(index)]}, 'yrs_edu', {[yrs_edu(index)]}, 'LearnEng', {{LearnEng{index}}})
                first=0;
            else
                info1.Sub_id=[info1.Sub_id;id];
                info1.Diag=[info1.Diag;diag];
                info1.Age=[info1.Age;ages(index)];
                info1.Gender=[info1.Gender;genders(index)];
                info1.yrs_edu=[info1.yrs_edu;yrs_edu(index)];
                info1.LearnEng=[info1.LearnEng;LearnEng{index}];
            end
        end
        info1
    end

    %writing classifications to file
    fid4=fopen(strcat('/cis/project/sydney/anova_analysis/combine_stats.txt'),'w');
    
%    save stat_files/texting.txt -struct info1 -ascii
        
    fprintf(fid4,'Sub_Id,Diag(0=norm,1=aMCI,2=nMCI),Age,Gender,YrsEd\n'); 
    fprintf(fid4,'%04u,%u,%.2f,%u,%u\n',[info1.Sub_id info1.Diag info1.Age info1.Gender info1.yrs_edu]')%,info1.LearnEng{:},'UniformOutput',false);
    
end
