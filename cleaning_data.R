library("hash")

replace_dict <- hash()

replace_dict[["Aciclovir"]] <- "Acyclovir"
replace_dict[['Colistimethate Sodium']] <- 'Polymyxin'
replace_dict[['Amoxicillin/potassium']] <- 'Amoxicillin'
replace_dict[['Cefpodoxmine Proxetil']] <- 'Cefpodoxime'
replace_dict[['Polymyxin B']] <- 'Polymyxin'
replace_dict[['Polymixin B']] <- 'Polymyxin'
replace_dict[['Colistin']] <- 'Polymyxin'
replace_dict[['Amphotericin B (Liposomal )']] <- 'Amphotericin B'
replace_dict[['Valaciclovir']] <- 'Valacyclovir'
replace_dict[['Valacyclovir ( Valacyclovir) ']] <- 'Valacyclovir'
replace_dict[['Cefipime']] <- 'Cefepime'
replace_dict[['Ceftazidime + Tazobactum']] <- 'Ceftazidime + Tazobactam'
replace_dict[['Meropenem ']] <- 'Meropenem'
replace_dict[['Capsofungin ']] <- 'Caspofungin'
replace_dict[['Gentamycin']] <- 'Gentamicin'
replace_dict[['Oseltamivir Phosphate']] <- 'Oseltamivir'
replace_dict[['Hydroxychloroquine/Hcqs']] <- 'Hydroxychloroquine'

##### DEFINING ANTIBIOTIC GROUPS #####

reserve_group <- c("Fosfomycin", "Linezolid", "Tigecycline","Daptomycin","Cefipime","Cefepime + Sulbactam","Cefepime + Tazobactam")
watch_group <- c("Ciprofloxacin", "Levofloxacin","Moxifloxacin","Norfloxacin", "Ofloxacin",
                 "Prulifloxacin","Levofloxacin","Ciprofloxacin","Cefixime","Ceftriaxone","Ceftriaxone/Sulbactam",
                 "Ceftriaxone/Tazobactam","Cefotaxime",
                 "Ceftazidime","Ceftazidime + Tazobactam","Ceftazidime/Avibactam","Azithromycin","Clarithromycin","Erythromycin",
                 "Vancomycin","Teicoplanin","Piperacillin/Tazobactam","Meropenem","Meropenem + Tazobactam","Meropenem/Sulbactam","Ertapenem","Faropenem",
                 "Doripenem")
abx_for_or <- c("Acyclovir", "Amikacin", "Amoxicillin", "Amoxicillin/Clavulanic Acid", "Amphotericin B",
                "Ampicillin", "Azithromycin", "Benzyl Pencillin", "Caspofungin", "Cefepime",
                "Cefepime + Sulbactam", "Cefixime", "Cefoperazone","Cefoperazone/sulbactam", "Cefotaxime", "Cefpodoxime",
                "Ceftazidime","Ceftazidime/Avibactam", "Ceftriaxone", "Ceftriaxone/Sulbactam", "Ceftriaxone/Tazobactam", "Cefuroxime",
                "Ciprofloxacin", "Clarithromycin", "Clindamycin", "Ertapenem", "Favipiravir", "Fluconazole", "Fosfomycin", "Gentamicin",
                "Imipenem/cilastatin", "Isavuconazole", "Itraconazole","Ivermectin", "Levofloxacin", "Linezolid", "Meropenem", "Meropenem/Sulbactam",
                " Micafungin", "Moxifloxacin", "Norfloxacin","Nitrofurantoin", "Norfloxacin", "Ofloxacin", "Oseltamivir","Piperacillin",
                "Piperacillin/Tazobactam", "Polymyxin", "Posaconazole", "Remdesivir", "Rifaximin", "Teicoplanin", "Tigecycline",
                "Vancomycin", "Voriconazole")
covid_specific_abx <- c("Azithromycin","Remdesivir","Favipiravir","Oseltamivir","Ivermectin")

more_than_50 <- c("Acyclovir", "Amikacin", "Amoxicillin/Clavulanic Acid", "Amphotericin B",
                  "Azithromycin","Caspofungin", "Cefepime",
                  "Cefepime + Sulbactam", "Cefixime","Cefoperazone/sulbactam", "Cefotaxime",
                  "Ceftriaxone", "Ceftriaxone/Tazobactam", "Cefuroxime",
                  "Ciprofloxacin", "Clarithromycin", "Clindamycin","Favipiravir", "Fluconazole",
                  "Imipenem/cilastatin","Levofloxacin", "Linezolid", "Meropenem","Ivermectin",
                  "Moxifloxacin","Nitrofurantoin","Oseltamivir","Piperacillin/Tazobactam", "Polymyxin", "Remdesivir", "Rifaximin", "Teicoplanin", "Tigecycline","Vancomycin", "Voriconazole", "Hydroxychloroquine")

access_group <- c('Amikacin','Amoxicillin','Amoxicillin/clavulanic acid', 'Ampicillin','Ampicillin sulbactam','Bacampicillin','Benzathine benzylpenicillin',	
                  'Benzylpenicillin','Cefacetrile','Cefadroxil','Cefalexin','Cefalotin','Cefapirin','Cefatrizine','Cefazedone','Cefazolin','Cefradine','Cefroxadine',
                  'Ceftezole','Chloramphenicol','Clindamycin','Clometocillin','Cloxacillin','Dicloxacillin','Doxycycline','Flucloxacillin','Gentamicin','Mecillinam',
                  'Metronidazole','Metronidazole','Nafcillin','Nitrofurantoin','Oxacillin','Penamecillin','Phenoxymethylpenicillin','Pivampicillin','Pivmecillinam',
                  'Procaine benzylpenicillin',	'Spectinomycin','Tetracycline','Thiamphenicol','Trimethoprim')

# Defining regions
northwest <- c("PB", "HR", "LA", "UP", "MP", "GJ")
east <- c("AS", "BR", "JH", "WB", "OD")
south <- c("AP", "KA", "KL", "TN", "MH")


