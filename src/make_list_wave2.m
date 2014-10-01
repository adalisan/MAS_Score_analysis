function make_list_wave2()

    rand('state',sum(100.*clock));
    %opening MCI classification document and list of ids for region (QC and
    %non_QC

    fid1=fopen('/cis/project/sydney/MAS W1and2_Diagnostic_classifications.csv','r');
    datatypes=textscan(fid1,'%1s%4s%s%n','Delimiter',',','HeaderLines',1);
    cell_ids=datatypes{2};
    ids=cellfun(@str2num,cell_ids);
    types=datatypes{4};


    fid2=fopen(strcat('/cis/project/sydney/anova_analysis/sydney_wave1_stats.txt'),'r');
    headerLine=textscan(fid2,'%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s%s',1,'Delimiter',',');
    datalist=textscan(fid2,'%n%n%f%n%f%s%s%f%f%f%f%f%f%f%f%f%f%f%f','Delimiter',',');
    mri_ids=datalist{1};

    diag2=[];
    
    for n=1:size(mri_ids,1)
        id=mri_ids(n);
        index=find(ids==id);
        if size(index,1)==0;
            diag2=[diag2;666];
        else type=types(index);
            diag2=[diag2;type];
        end
    end

    headerLine{20}='Diag2';
    datalist{20}=diag2;
    
    %writing classifications to file
    fid4=fopen('/cis/project/sydney/anova_analysis//sydney_wave2_stats.txt','w');
    headerLine{1};
    fprintf(fid4,'%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s\n',headerLine{1}{1},headerLine{2}{1},headerLine{3}{1},headerLine{4}{1},headerLine{5}{1},headerLine{6}{1},headerLine{7}{1},headerLine{8}{1},headerLine{9}{1},headerLine{10}{1},headerLine{11}{1},headerLine{12}{1},headerLine{13}{1},headerLine{14}{1},headerLine{15}{1},headerLine{16}{1},headerLine{17}{1},headerLine{18}{1},headerLine{19}{1},headerLine{20})
    for i=1:size(datalist{1},1)
        fprintf(fid4,'%u,%u,%f,%u,%f,%s,%s,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%f,%u\n',datalist{1}(i),datalist{2}(i),datalist{3}(i),datalist{4}(i),datalist{5}(i),datalist{6}{i},datalist{7}{i},datalist{8}(i),datalist{9}(i),datalist{10}(i),datalist{11}(i),datalist{12}(i),datalist{13}(i),datalist{14}(i),datalist{15}(i),datalist{16}(i),datalist{17}(i),datalist{18}(i),datalist{19}(i),datalist{20}(i));
    end    
    %normal_ids and MCI_ids_amnestic
end
