#Private renters who can't afford median market rent


############### Flags for afford rent ########################

# flag if can afford median market rent, using gross household income
affdt_1315[ , amount_canaff_g := (hh_grossinc*0.3)/12]
affdt_1315[ nohbai_inc == 0, aff_rentmed_g := ifelse(   amount_canaff_g >= rent_med, 1, 0 )]
affdt_1315[ nohbai_inc == 0, aff_rentlq_g := ifelse(   amount_canaff_g >= rent_lq, 1, 0 )]

# flag is can afford median market rent, using gross household income
affdt_1315[ multiple_benu == 0, amount_canaff_gnohb := (hh_grossinc_nohb*0.3)/12]
affdt_1315[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentmed_gnohb := ifelse(   amount_canaff_gnohb >= rent_med, 1, 0 )]
affdt_1315[ (multiple_benu == 0 & nohbai_inc == 0), aff_rentlq_gnohb := ifelse(   amount_canaff_gnohb >= rent_lq, 1, 0 )]

#aff.labels <- sji.getValueLabels(affdt_1315_1BU)


#with HB
affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentmed_g, weight.by = hhweight_2)]
affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentlq_g, weight.by = hhweight_2)]

#without HB
affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentmed_gnohb, weight.by = hhweight_2)]
affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.frq(aff_rentlq_gnohb, weight.by = hhweight_2)]

#Cross tabs
affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
                ]

affdt_1315_1BU[ ( (PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP), sjt.xtab(aff_rentlq_g, GVTREGN,
                                                                      var.labels=c("Can afford LQ rent", "Region"), 
                                                                      weight.by = hhweight_2, show.col.prc = TRUE)
                ]

#Social renters
#Cross tabs
affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2) & is_HRP ), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
                ]

affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2) & is_HRP ), sjt.xtab(aff_rentlq_g, GVTREGN, var.labels=c("Can afford LQ rent", "Region"), 
                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
                ]

#All renters
affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP), sjt.xtab(aff_rentmed_g, GVTREGN, var.labels=c("Can afford med rent", "Region"),
                                                                                                      weight.by = hhweight_2, show.col.prc = TRUE)
                ]

affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP ), sjt.xtab(aff_rentlq_g, GVTREGN, var.labels=c("Can afford LQ rent", "Region"), 
                                                                                                       weight.by = hhweight_2, show.col.prc = TRUE)
                ]


#Household characteristics
affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP & GVTREGN == "London"), sjt.xtab(aff_rentmed_g, SELFDEMP, var.labels=c("Can afford med rent", "Region"),
                                                                                                                            weight.by = hhweight_2, show.row.prc = TRUE)
                ]

affdt_1315_1BU[ ( (PTENTYP2 == 1 | PTENTYP2 == 2 | PTENTYP2 == 3 | PTENTYP2 == 4) & is_HRP & GVTREGN != "London"), sjt.xtab(aff_rentmed_g, SELFDEMP, var.labels=c("Can afford med rent", "Region"),
                                                                                                                            weight.by = hhweight_2, show.row.prc = TRUE)
                ]


