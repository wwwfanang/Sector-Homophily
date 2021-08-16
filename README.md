# Sector-Homophily
This repository contains data and codes we used in support of our work "Sector Homophily Levels in Vocational Networks Across 20 Years".

This project provides the dataset of mobility network structure, network community, and companies with GICS code. In order to protect the privacy, the companies' name are all hashes by sha256. And also provides the code of the main figures in our paper.

The folder "Data" provides the data of the mobility network. Split into multiple files due to file size limitation. According to the column name of file, they are the worker's ID, and the information of his first job(including work period, job title, company name, company size and job grade), tthe information of the second job, and the time of job change.

The file "Company_Community_2015_GC_Louvain.zip.001" and "Company_Community_2015_GC_Louvain.zip.002" provide the community of each company we detected by Louvain. The first column is the coded company name, and the second is the community number.

The file "Company_Sector_Community1.csv" and "Company_Sector_Community2.csv" provide the sector of comapnies. Each column in turn is the coded comapny name, community number and sector.

The rest of the R codes can correspond to the figures in the paper according to the file name.
