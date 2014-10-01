function combine_stats_2()

    fid2=fopen('/cis/project/sydney/anova_analysis/combine_stats.txt');
    id_stats=textscan(fid2,'%n%n%f%n%f','Delimiter',',','HeaderLines',1);
    ids_diag=id_stats{1};
    
    fid3=fopen(strcat('/cis/project/sydney/anova_analysis/sydney_wave1_stats.txt'),'w');
    fprintf(fid3,'"Sub_Id","Diag","Age","Gender","YrsEd","Hemi","Region","T95","T99","V95","V99","S","ICV","T95_2","T99_2","V95_2","V99_2","S_2","ICV_2"\n');
    
    hemis={'rh','lh'};
    subregions={'stg','mtg','itg','antcing','postcing'};
    for subregion=subregions
        for hemi=hemis
            subregion
            hemi
            %%% GETTING WAVE1 DATA %%%

            fid1a=fopen(strcat('/cis/project/sydney/data_analysis_MAS.20100921/',hemi{1},'_',subregion{1},'/',hemi{1},'_',subregion{1},'_lcdmstats.csv'),'r');
            fid1b=fopen(strcat('/cis/project/sydney/data_analysis_MAS.20110324/',hemi{1},'_',subregion{1},'/',hemi{1},'_',subregion{1},'_lcdmstats.csv'),'r');

            headers_a=textscan(fid1a,'%s%s%s%s%s%s%s',1,'Delimiter',' ');
            data_a=textscan(fid1a,'%5s%s%f%f%f%f%f%f','Delimiter',' ');
            headers_b=textscan(fid1b,'%s%s%s%s%s%s%s',1,'Delimiter',' ');
            data_b=textscan(fid1b,'%5s%s%f%f%f%f%f%f','Delimiter',' ');

            ids_temp=strrep(data_a{1},'"','');
            ids_a=cellfun(@str2num,ids_temp);
            ids_temp=strrep(data_b{1},'"','');
            ids_b=cellfun(@str2num,ids_temp);
            ids_all=[ids_a;ids_b];

            thick_95_data=[data_a{3};data_b{3}];
            thick_99_data=[data_a{4};data_b{4}];
            vol_95_data=[data_a{5};data_b{5}];
            vol_99_data=[data_a{6};data_b{6}];
            surf_data=[data_a{7};data_b{7}];
 	     ICV_data=[data_a{8};data_b{8}];

	     %GETTING WAVE2 DATA %%%

	     fid2=fopen(strcat('/cis/project/sydney/data_analysis_wave2/',hemi{1},'_',subregion{1},'/',hemi{1},'_',subregion{1},'_lcdmstats.csv'),'r');

 	     headers_2=textscan(fid2,'%s%s%s%s%s%s%s',1,'Delimiter',' ');
	     data_2=textscan(fid2,'%5s%s%f%f%f%f%f%f','Delimiter',' ');

	     ids_2_temp=strrep(data_2{1},'"','');
	     ids_2=cellfun(@str2num,ids_2_temp);

	     thick_95_data_2=data_2{3};
            thick_99_data_2=data_2{4};
            vol_95_data_2=data_2{5};
            vol_99_data_2=data_2{6};
            surf_data_2=data_2{7}
 	     ICV_data_2=data_2{8};


            %%%  INITIALIZE DATA VECTORS

            thick_95=[];
            thick_99=[];
            vol_95=[];
            vol_99=[];
            surf=[];
            ICV=[];

            thick_95_2=[];
            thick_99_2=[];
            vol_95_2=[];
            vol_99_2=[];
            surf_2=[];
            ICV_2=[];         
	     i_temp=1;
    for n=1:size(ids_diag,1)
              id=ids_diag(n);
              index=find(ids_all==id);
              thick_95=[thick_95 thick_95_data(index)];
              vol_95=[vol_95 vol_95_data(index)];
              thick_99=[thick_99 thick_99_data(index)];
              vol_99=[vol_99 vol_99_data(index)];
              surf=[surf surf_data(index)];
              ICV=[ICV ICV_data(index)];               		
            index2=find(ids_2==id)
     		if size(index2,1)==0
                thick_95_2=[thick_95_2 NaN];
	            vol_95_2=[vol_95_2 NaN];
              	thick_99_2=[thick_99_2 NaN];
              	vol_99_2=[vol_99_2 NaN];
              	surf_2=[surf_2 NaN];
              	ICV_2=[ICV_2 NaN];               
            else
        		thick_95_2=[thick_95_2 thick_95_data_2(index2)];
              	vol_95_2=[vol_95_2 vol_95_data_2(index2)];
              	thick_99_2=[thick_99_2 thick_99_data_2(index2)];
              	vol_99_2=[vol_99_2 vol_99_data_2(index2)];
              	surf_2=[surf_2 surf_data_2(index2)];
              	ICV_2=[ICV_2 ICV_data_2(index2)];               
            end
    end
    

            if strcmp(subregion,'stg') && strcmp(hemi,'rh')
                rhstg_age=id_stats{4};
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,r,stg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'stg') && strcmp(hemi,'lh')
                lhstg_age=id_stats{4};
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,l,stg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']'); 
            elseif strcmp(subregion,'mtg') && strcmp(hemi,'rh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,r,mtg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'mtg') && strcmp(hemi,'lh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,l,mtg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'itg') && strcmp(hemi,'rh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,r,itg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'itg') && strcmp(hemi,'lh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,l,itg,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'antcing') && strcmp(hemi,'rh')
			%thick_95_2
			data_2{6}
			%ids_all
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,r,antcing,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'antcing') && strcmp(hemi,'lh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,l,antcing,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'postcing') && strcmp(hemi,'rh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,r,postcing,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            elseif strcmp(subregion,'postcing') && strcmp(hemi,'lh')
                fprintf(fid3,'%04u,%u,%.2f,%u,%.2f,l,postcing,%.4f,%.4f,%u,%u,%.2f,%.6f,%.4f,%.4f,%u,%u,%.2f,%.6f\n',[id_stats{1} id_stats{2} id_stats{3} id_stats{4} id_stats{5} thick_95' thick_99' vol_95' vol_99' surf' ICV' thick_95_2' thick_99_2' vol_95_2' vol_99_2' surf_2' ICV_2']');
            end
        end
    end
    
    
end
