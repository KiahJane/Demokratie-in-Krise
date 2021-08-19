############################################################################################################################################
#   Demokratie in Krise (BG, DE, HR)                                                                                                       #
#      --> Viktor Stanic, Stanko Karamollov, Dominic Mauersberger                                                                          #
############################################################################################################################################
  
## packages
install.packages('tidyverse')
library(tidyverse)
library(dplyr)
install.packages('ggstatsplot')
library(ggstatsplot)
library(ggplot2)
install.packages('labelled')
library(labelled)
install.packages('reshape2')
library(reshape2)
install.packages('car')
library(car)

###########################################################################################################################################
## import data
datensatz_DE_1 <- read.csv("C:/Users/Matija/Downloads/Analysis.Boys/Deutschland Fragebogen 1.csv", sep = ";") 
datensatz_DE_2 <- read.csv("C:/Users/Matija/Downloads/Analysis.Boys/Deutschland Fragebogen 2.csv", sep = ";")
datensatz_BG <- read.csv("C:/Users/Matija/Downloads/Analysis.Boys/Bulgarien Fragebogen.csv", sep = ";")
datensatz_HR <- read.csv("C:/Users/Matija/Downloads/Analysis.Boys/Kroatien Fragebogen.csv", sep = ";")

## Clean Up
# edit Datasets: rename columns to match; change variable types to match across datasets; add country variable
datensatz_DE_1 <- datensatz_DE_1 %>%
  rename(mitglied.partei = X1..1..Sind.Sie.Mitglied.einer.Partei..eines.Verbandes.oder.einer.Bewegung.,
         aktiv.partei = X2..2..Wie.aktiv.sind.Sie.in.der.Regel.in.Ihrer.Partei.Bewegung.,
         wahl.parlament = X3..3..Haben.Sie.im.Jahr.2017.bei.der.Bundestagswahl.in.Deutschland.gewÃ.hlt.,
         wahl.2019 = X4..4..Haben.Sie.bei.der.Europawahl.2019.gewÃ.hlt.,
         zufr.system = X5..5..Wie.zufrieden.sind.mit.dem.politischen.System.der.Bundesrepublik.Deutschland.,
         gg.ander = X6..6..Sehen.Sie.im.politischen.System.Deutschlands.mehr.Vorteile.gegenÃ.ber.anderen.Regierungsformen.,
         partei = X7..7..Welche.Partei.haben.Sie.bei.der.letzten.Bundestagswahl.2017.in.Deutschland.gewÃ.hlt.,
         reg.partei = X8..8..WÃ.hlen.Sie.diese.Partei.regelmÃ.ÃŸig.,
         next.wahl.partei = X9..9..WÃ.rden.Sie.diese.Partei.bei.den.nÃ.chsten.Parlamentswahlen.wÃ.hlen.,
         grund = X10..Aus.welchen.GrÃ.nden.wÃ.hlen.wÃ.hlten.Sie.diese.politische.Partei.,
         teil.veran = X11..10..Nehmen.Sie.an.politischen.Veranstaltungen.teil.,
         partei.interes = X12..11..Sind.Sie.der.Meinung..dass.die.von.Ihnen.gewÃ.hlte.Partei.Ihre.Interessen.gewissenhaft.vertritt.,
         aktiv.tag = X13..12..Wie.aktiv.verfolgen.Sie.die.Tagespolitik.,
         info.wahl = X14..13..Informieren.Sie.sich.zusÃ.tzlich.vor.Wahlen..Falls.Ja..wie.umfassend.,
         info.quelle = X15..14..Wie.informieren.Sie.sich.Ã.ber.politische.Themen.,
         wahrnehm = X16..15..FÃ.hlen.Sie.sich.von.der.Politik.wahrgenommen.,
         bezeug.partei = X17..1..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Parteien.Ihre.Pflichten.erfÃ.llen.und.die.Interessen.ihrer.WÃ.hler.vertreten.,
         bezeug.kanzler = X18..2..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Bundeskanzlerin.ihre.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.prez = X19..3..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.BundesprÃ.sident.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.btag = X20..4..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.Bundestag.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.justia = X21..5..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Judikative..Gerichte..Ihre.Pflichten.neutral..objektiv.und.unabhÃ.ngig.von.der.Politik.ausfÃ.hrt.,
         fair.behandl = X22..6..Denken.Sie..dass.Sie.vor.Gericht.eine.faire.Behandlung..im.Vergleich.zu.elitÃ.ren.Personen.aus.Wirtschaft.und.Politik..erhalten.wÃ.rden,
         sicher.heim = X23..7..Wie.sicher.fÃ.hlen.Sie.sich.in.Ihrer.Heimatstadt.,
         ver.freund = X24..8..Wie.sehr.vertrauen.Sie.Ihren.Freunden.,
         ver.fam = X25..9..Wie.sehr.vertrauen.Sie.Ihrer.Familie.,
         poli.moral = X26..10..Denken.Sie.die.meisten.Politiker.halten.sich.an.Gesetze.und.moralische.Normen.,
         poli.recht = X27..11..Wie.sehr.vertrauen.Sie.der.Rechtschaffenheit.von.Politikern.,
         poli.dien = X28..12..Denken.Sie..dass.Politiker.eher..den.MÃ.chtigen..als.der.eigenen.WÃ.hlerschaft.dienen.,
         ver.mitb = X29..13..Wie.sehr.vertauen.Sie.Ihren.MitbÃ.rgern.,
         off.gesell = X30..14..Empfinden.Sie.Ihre.Gesellschaft.als.offen.fÃ.r.andere.Meinungen.,
         sta = X31..Welche.StaatsangehÃ.rigkeiten.besitzen.Sie.,
         netto.einkom = X32..Wie.hoch.ist.Ihr.durschnittliches.monatliches.Nettoeinkommen.,
         finanz.sicher = X33..Wie.gut.fÃ.hlen.Sie.sich.finanziell.abgesichert.,
         alter = X34..Wie.alt.sind.Sie.,
         geschlecht = X35..Welches.Geschlecht.besitzen.Sie.,
         zeitung = Zeitungen.Zeitschriften,
         fernsehen = Fernsehen,
         medien = Medien..Facebook..Twitter..etc..,
         internet = Internet,
         radio = Radio,
         poli.akteuren = Treffen.mit.politischen.Akteuren,
         other = Textfeld) %>%
  mutate(zufr.system = as.numeric(zufr.system, na.rm = TRUE)) %>%
  add_column(land = "Deutschland") %>%
  mutate(wahl.parlament = recode(wahl.parlament, 'Ich war noch nicht wahlberechtigt' = "nicht wahlberechtigt")) 

datensatz_DE_2 <- datensatz_DE_2 %>%
  rename(mitglied.partei = X1..1..Sind.Sie.Mitglied.einer.Partei..eines.Verbandes.oder.einer.Bewegung.,
         aktiv.partei = X2..2..Wie.aktiv.sind.Sie.in.der.Regel.in.Ihrer.Partei.Bewegung.,
         wahl.parlament = X3..3..Haben.Sie.2017.bei.der.Bundestagswahl.in.Deutschland.gewÃ.hlt.,
         wahl.2019 = X4..4..Haben.Sie.bei.der.Europawahl.2019.gewÃ.hlt.,
         zufr.system = X5..5..Wie.zufrieden.sind.Sie.mit.dem.poltischen.System.der.Bundesrepublik.Deutschland.,
         gg.ander = X6..6..WÃ.rden.Sie.sich.eine.grÃ.ÃŸere.Einbeziehung.des.Volkes.in.die.Politik.wÃ.nschen...z.B.durch.Volksabstimmungen.,
         partei = X7..7..Welche.Partei.haben.Sie.bei.der.letzten.Bundestagswahl.2017.in.Deutschland.gewÃ.hlt.,
         reg.partei = X8..8..WÃ.hlen.Sie.diese.Partei.regelmÃ.ÃŸig.,
         next.wahl.partei = X9..9..WÃ.rden.Sie.diese.Partei.bei.den.nÃ.chsten.Bundestagswahlen.wieder.wÃ.hlen.,
         teil.veran = X10..10..Nehmen.Sie.an.politischen.Veranstaltungen.teil.,
         partei.interes = X11..11..Sind.Sie.der.Meinung..dass.die.politischen.Parteien.die.Interessen.ihrer.WÃ.hler.gewissenhaft.vertreten.,
         aktiv.tag = X12..12..Wie.aktiv.verfolgen.Sie.die.Tagespolitik.,
         info.wahl = X13..13..Informieren.Sie.sich.zusÃ.tzlich.vor.Wahlen..Falls.Ja..wie.umfassend.,
         info.quelle = X14..14..Wie.informieren.Sie.sich.Ã.ber.politische.Themen.,
         wahrnehm = X15..15..FÃ.hlen.Sie.sich.von.der.Politik.wahrgenommen.,
         bezeug.partei = X16..1..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Parteien.Ihre.Pflichten.erfÃ.llen.und.die.Interessen.ihrer.WÃ.hler.vertreten.,
         bezeug.kanzler = X17..2..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Bundeskanzlerin.ihre.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.prez = X18..3..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.BundesprÃ.sident.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.btag = X19..4..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.Bundestag.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.justia = X20..5..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Judikative..Gerichte..Ihre.Pflichten.neutral..objektiv.und.unabhÃ.ngig.von.der.Politik.ausfÃ.hrt.,
         fair.behandl = X21..6..Denken.Sie..dass.Sie.vor.Gericht.eine.faire.Behandlung..im.Vergleich.zu.elitÃ.ren.Personen.aus.Wirtschaft.und.Politik..erhalten.wÃ.rden,
         sicher.heim = X22..7..Wie.sicher.fÃ.hlen.Sie.sich.in.Ihrer.Heimatstadt.,
         ver.freund = X23..8..Wie.sehr.vertrauen.Sie.Ihren.Freunden.,
         ver.fam = X24..9..Wie.sehr.vertrauen.Sie.Ihrer.Familie.,
         poli.moral = X25..10..Denken.Sie.die.meisten.Politiker.halten.sich.an.Gesetze.und.moralische.Normen.,
         poli.recht = X26..11..Wie.sehr.vertrauen.Sie.der.Rechtschaffenheit.von.Politikern.,
         poli.dien = X27..12..Denken.Sie..dass.Politiker.eher..den.MÃ.chtigen..als.der.eigenen.WÃ.hlerschaft.dienen.,
         ver.mitb = X28..13..Wie.sehr.vertauen.Sie.Ihren.MitbÃ.rgern.,
         off.gesell = X29..14..Empfinden.Sie.Ihre.Gesellschaft.als.offen.fÃ.r.andere.Meinungen.,
         sta = X30..Welche.StaatsangehÃ.rigkeiten.besitzen.Sie.,
         netto.einkom = X31..Wie.hoch.ist.Ihr.durschnittliches.monatliches.Nettoeinkommen.,
         finanz.sicher = X32..Wie.gut.fÃ.hlen.Sie.sich.finanziell.abgesichert.,
         alter = X33..Wie.alt.sind.Sie.,
         geschlecht = X34..Welches.Geschlecht.besitzen.Sie.,
         zeitung = Zeitungen.Zeitschriften,
         fernsehen = Fernsehen,
         medien = Medien..Facebook..Twitter..etc..,
         internet = Internet,
         radio = Radio,
         poli.akteuren = Treffen.mit.politischen.Akteuren,
         other = Textfeld) %>%
  mutate(zufr.system = as.numeric(zufr.system, na.rm = TRUE)) %>%
  add_column(land = "Deutschland") %>%
  mutate(wahl.parlament = recode(wahl.parlament, 'Ich war noch nicht wahlberechtigt' = "nicht wahlberechtigt")) 

datensatz_BG <- datensatz_BG %>%
  rename(mitglied.partei = X1..1..Sind.Sie.Mitglied.einer.Partei..eines.Verbandes.oder.einer.Bewegung.,
         aktiv.partei = X2..2..Wie.aktiv.sind.Sie.in.der.Regel.in.Ihrer.Partei.Bewegung.,
         wahl.parlament = X3..3..Haben.Sie.im.Jahr.2017.bei.der.Parlamentswahl.in.Bulgarien.gewÃ.hlt.,
         wahl.2019 = X4..4..Haben.Sie.bei.der.Europawahl.2019.gewÃ.hlt.,
         zufr.system = X5..5..Wie.zufrieden.sind.mit.dem.poltischen.System.der.Republik.Bulgarien.,
         gg.ander = X6..6..Sehen.Sie.im.politischen.System.Bulgariens.mehr.Vorteile.gegenÃ.ber.andern.Regierungsformen.,
         partei = X7..7..Welche.Partei.haben.Sie.bei.der.letzten.Parlamentswahl.2017.in.Bulgarien.gewÃ.hlt.,
         reg.partei = X8..8..WÃ.hlen.Sie.diese.Partei.regelmÃ.ÃŸig.,
         next.wahl.partei = X9..9..WÃ.rden.Sie.diese.Partei.bei.den.nÃ.chsten.Parlamentswahlen.wÃ.hlen.,
         teil.veran = X10..10..Nehmen.Sie.an.politischen.Veranstaltungen.teil.,
         partei.interes = X11..11..Sind.Sie.der.Meinung..dass.die.Partei.Ihre.Interessen.gewissenhaft.vertritt.,
         aktiv.tag = X12..12..Wie.aktiv.verfolgen.Sie.die.Tagespolitik.,
         info.wahl = X13..13..Informieren.Sie.sich.zusÃ.tzlich.vor.Wahlen..Falls.Ja..wie.umfassend.,
         info.quelle = X14..14..Wie.informieren.Sie.sich.Ã.ber.politische.Themen.,
         wahrnehm = X15..15..FÃ.hlen.Sie.sich.von.der.Politik.wahrgenommen.,
         bezeug.partei = X16..1..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Parteien.Ihre.Pflichten.erfÃ.llen.und.die.Interessen.ihrer.WÃ.hler.vertreten.,
         bezeug.kanzler = X17..2..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.MinisterprÃ.sident.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.prez = X18..3..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.der.PrÃ.sident.seine.Pflichten.gewisenhaft.erfÃ.llt.,
         bezeug.btag = X19..4..Wie.sehr.sind.Sie.dabvon.Ã.berzeugt..dass.das.Parlament.seine.Pflichten.gewissenhaft.erfÃ.llt.,
         bezeug.justia = X20..5..Wie.sehr.sind.Sie.davon.Ã.berzeugt..dass.die.Judikative..Gerichte..Ihre.Pflichten.neutral..objektiv.und.unabhÃ.ngig.von.der.Politik.ausfÃ.hrt.,
         fair.behandl = X21..6..Denken.Sie..dass.Sie.vor.Gericht.eine.faire.Behandlung..im.Vergleich.zu.elitÃ.ren.Personen.aus.Wirtschaft.und.Politik..erhalten.wÃ.rden,
         sicher.heim = X22..7..Wie.sicher.fÃ.hlen.Sie.sich.in.Ihrer.Hematstadt.,
         ver.freund = X23..8..Wie.sehr.vertrauen.Sie.Ihren.Freunden.,
         ver.fam = X24..9..Wie.sehr.vertrauen.Sie.Ihrer.Familie.,
         poli.moral = X25..10..Denken.Sie.die.meisten.Politiker.halten.sich.an.Gesetze.und.moralische.Normen.,
         poli.recht = X26..11..Wie.sehr.vertrauen.Sie.der.Rechtschaffenheit.von.Politikern.,
         poli.dien = X27..12..Denken.Sie..dass.Politiker.eher..den.MÃ.chtigen..als.der.eigenen.WÃ.hlerschaft.dienen.,
         ver.mitb = X28..13..Wie.sehr.vertauen.Sie.Ihren.MitbÃ.rgern.,
         off.gesell = X29..14..Empfinden.Sie.Ihre.Gesellschaft.als.offen.fÃ.r.andere.Meinungen.,
         sta = X30..Welche.StaatsangehÃ.rigkeiten.besitzen.Sie.,
         netto.einkom = X31..Wie.hoch.ist.Ihr.durschnittliches.monatliches.Nettoeinkommen.,
         finanz.sicher = X32..Wie.gut.fÃ.hlen.Sie.sich.finanziell.abgesichert.,
         alter = X33..Wie.alt.sind.Sie.,
         geschlecht = X34..Welches.Geschlecht.besitzen.Sie.,
         zeitung = Zeitungen.Zeitschriften,
         fernsehen = Fernsehen,
         medien = Medien..Facebook..Twitter..etc..,
         internet = Internet,
         radio = Radio,
         poli.akteuren = Treffen.mit.politischen.Akteuren,
         other = Textfeld) %>%
  mutate(zufr.system = as.numeric(zufr.system, na.rm = TRUE), ver.mitb = as.integer(ver.mitb, na.rm = TRUE)) %>%
  add_column(land = "Bulgarien") %>%
  mutate(wahl.parlament = recode(wahl.parlament, 'Ich war noch nicht wahlberechtigt' = "nicht wahlberechtigt")) 

datensatz_HR <- datensatz_HR %>%
  rename(mitglied.partei = X1..1..Jeste.li.Ä.lan.jedne.politiÄ.ke.stranke..udruge.ili.dio.nekog.politiÄ.kog.pokreta.,
         aktiv.partei = X2..2..Koliko.Ä.esto.sudjelujete.u.aktivnostima.u.vaÅ.oj.stranci.udruzi.pokretu.,
         wahl.parlament = X3..3..Jeste.li.glasovali.na.izborima.za.Hrvatski.Sabor.u.srpnju.2020..godine.,
         wahl.2019 = X4..4..Jeste.li.glasali.na.izborima.za.Europski.parlament.2019..godine.,
         wahl.2017 = X5..5..Jeste.li.2017..godine.glasovali.na.izborima.za.predstavniÄ.ka.tijela.u.vaÅ.oj.Å.upaniji.,
         wahl.2017.stadt = X6..6..Jeste.li.2017..godine.glasovali.na.izborima.za.predstavniÄ.ka.tijela.u.vaÅ.oj.opÄ.ini.gradu.,
         zufr.system = X7..7..Koliko.ste.zadovoljni.politiÄ.kim.sustavom.Republike.Hrvatske.,
         gg.ander = X8..8..U.odnosu.na.neke.druge.sustave.vlasti.i.upravljanja..vidite.li.hrvatsko.politiÄ.ko.ureÄ.enje.ipak.pozitivnijim.modelom.vlasti.,
         partei = X9..9..Za.koju.ste.stranku.koaliciju.glasali.na.parlamentarnim.izborima.u.srpnju.2020..godine.,
         reg.partei = X10..10..Birate.li.gore.navedenu.politiÄ.ku.opciju.u.kontinuitetu.,
         next.wahl.partei = X11..11..HoÄ.ete.li.za.tu.stranku.opciju.glasovati.i.na.iduÄ.im.izborima.za.Hrvatski.sabor.,
         grund = X12..12..Iz.kojeg.razloga.birate.gore.navedenu.politiÄ.ku.opciju.,
         teil.veran = X13..12..Sudjelujete.li.na.nekoj.vrsti.politiÄ.kih.dogaÄ.anja..politiÄ.ki.prosvjedi..potpisivanje.peticija.i.sl...,
         aktiv.tag = X14..13..Koliko.aktivno.i.Ä.esto.pratite.dnevnopolitiÄ.ka.zbivanja.,
         info.wahl = X15..14..U.kolikoj.se.mjeri.informirate.pred.izbore.,
         info.zeit = X16..15..Koliko.vremena.tjedno.iskoristite.za.informiranje.o.politiÄ.kim.zbivanjima.preko.medija.,
         info.quelle = X17..16..Na.koji.se.naÄ.in.informirate.o.politici.i.politiÄ.kim.dogaÄ.ajima.,
         wahrnehm = X18..17..OsjeÄ.ate.li.se.dovoljno.zastupljeno.u.politici.,
         bezeug.partei = X19..1..U.kojoj.mjeri.mislite.da.politiÄ.ke.stranke.savjesno.ispunjavaju.svoju.osnovnu.duÅ.nost..zastupanje.interesa.svog.biraÄ.kog.tijela..,
         bezeug.prez = X20..2..U.kojoj.mjeri.dijelite.miÅ.ljenje.s.pretpostavkom..da.Predsjednik.Vlade.RH.Ä.asno.i.savjesno.ispunjava.svoje.duÅ.nosti.,
         bezeug.btag = X21..3..U.kojoj.mjeri.dijelite.miÅ.ljenje.s.pretpostavkom..da.Hrvatski.Sabor.Ä.asno.i.savjesno.ispunjava.svoje.duÅ.nosti.,
         bezeug.justia = X22..4..U.kojoj.mjeri.dijelite.miÅ.ljenje.s.pretpostavkom..da.sudbena.vlast..prvenstveno.sudovi..svoje.odluke.donosi.neutralno..objektivno.i.bez.politiÄ.kog.utjecaja.,
         fair.behandl = X23..5..U.sluÄ.aju.sudskog.procesa..mislite.li.da.bi.vam.se.sudilo.na.isti.naÄ.in.kao.politiÄ.kim.i.ekonomskim.elitama.u.Hrvatskoj.,
         sicher.heim = X24..6..Koliko.se.sigurno.osjeÄ.ate.u.vaÅ.em.mjestu.prebivaliÅ.ta.ili.gradu.,
         ver.freund = X25..7..U.kolikoj.mjeri.vjerujete.svojim.prijateljima.,
         ver.fam = X26..8..U.kojoj.mjeri.vjerujete.svojoj.obitelji.,
         poli.moral = X27..9..Mislite.li.da.se.veÄ.ina.politiÄ.ara.pridrÅ.ava.zakona.i.opÄ.ih.moralnih.naÄ.ela.,
         poli.recht = X28..10..U.kojoj.mjeri.vjerujete.u.poÅ.tenje.i.praviÄ.nost.politiÄ.ara.,
         poli.dien = X29..11..Mislite.li.da.politiÄ.ari.viÅ.e.podlijeÅ.u.interesima.raznih.moÄ.nika.nego.svom.biraÄ.kom.tijelu.,
         ver.mitb = X30..12..U.kojoj.mjeri.vjerujete.svojim.sugraÄ.anima.,
         off.gesell = X31..13..Smatrate.li.hrvatsko.druÅ.tvo.otvorenim.za.razliÄ.ita.miÅ.ljenja.,
         sta = X32..1..DrÅ.avljanstvo.,
         alter = X33..2..Dob.,
         geschlecht = X34..3..Spol.,
         netto.einkom = X35..4..MjeseÄ.na.primanja.,
         finanz.sicher = X36..5..Kako.bi.opisali.vaÅ.u.financijsku.situaciju.,
         zeitung = Dnevne.novine.i.tjednici,
         medien = DruÅ.tvene.mreÅ.e..Facebook..Twitter.i.sl..,
         fernsehen = Televizija,
         internet = Internet,
         radio = Radio,
         poli.akteuren = Susreti.s.politiÄ.kim.akterima,
         other = Textfeld) %>%
  mutate(zufr.system = as.numeric(zufr.system, na.rm = TRUE), ver.mitb = as.integer(ver.mitb, na.rm = TRUE)) %>%
  add_column(land = "Kroatien") %>%
  mutate(mitglied.partei = recode(mitglied.partei, 'Da' = "Ja", 'Bez navoda' = "Keine Angabe", 'Ne' = "Nein")) %>%
  mutate(reg.partei = recode(reg.partei, 'Da' = "Ja", 'Bez navoda' = "Keine Angabe", 'Ne' = "Nein")) %>%
  mutate(wahl.parlament = recode(wahl.parlament, 'Da' = "Ja", 'Nisam imao glasaÄko pravo' = "nicht wahlberechtigt", 'Ne' = "Nein")) 
  
# merge all Datasets 
datensatz_DE <- merge(datensatz_DE_1, datensatz_DE_2, all = TRUE)
datensatz_DE_BG <- full_join(datensatz_DE, datensatz_BG)
datensatz_alle <- full_join(datensatz_DE_BG, datensatz_HR) 

# Edit joined dataset: remove useless variables; make data uniform
datensatz_alle <- datensatz_alle %>%
  select(-ï.._Antwort.ID, -Resume.Code, -Start, -Datum.und.Zeit, -Teilnahmestatus, -info.quelle) %>%
  mutate(finanz.sicher = recode(finanz.sicher, 
                          'Ich fÃ¼hle mich finanziell nicht abgesichert' = "nicht abgesichert",
                          'Finanziell kÃ¶nnte ich besser abgesichert sein' = "mittelmäßig",
                          'Finanziell fÃ¼hle ich mich ausreichend abgesichert' = "ausreichend",
                          'Finanziell fÃ¼hle ich mich sehr gut abgesichert' = "sehr gut",
                          'keine Angabe' = "Keine Angabe",
                          'Student' = "student", 
                          'Nesigurno' = "nicht abgesichert",
                          'Vrlo nesigurno' = "nicht abgesichert",
                          'Moglo bi biti bolje' = "mittelmäßig", 
                          'Dovoljno sigurno' = "ausreichend",
                          'Zivim bolje nog ministar' = "sehr gut", 
                          'Vrlo sigurno' = "sehr gut",
                          'student' = "student",
                          'Bez navoda' = "Keine Angabe")) %>%
  mutate(aktiv.tag = recode(aktiv.tag, 
                            'Manje od sat vremena tjedno' = "weniger als 1",
                            'Oko tri sata tjedno' = "weniger als 3",
                            'Oko pet sati tjedno' = "weniger als 5",
                            'ViÅ¡e od osam sati tjedno' = "mehr als 8",
                            'Weniger als eine Stunde in der Woche' = "weniger als 1",
                            'Bis zu drei Stunden in der Woche' = "weniger als 3",
                            'Bis zu fÃ¼nf Stunden in der Woche' = "weniger als 5",
                            'Mehr als acht Stunden in der Woche' = "mehr als 8",
                            'bez navoda' = "keine Angabe")) %>%
  mutate(off.gesell = recode(off.gesell, 
                             'Da' = "Ja",
                             'Ne' = "Nein",
                             'Bez navoda' = "keine Angabe")) %>%
  mutate(next.wahl.partei = recode(next.wahl.partei, 
                             'Da' = "Ja",
                             'Ne' = "Nein",
                             'Bez navoda' = "keine Angabe",
                             'Keine Angabe' = "keine Angabe")) %>%
  mutate(reg.partei = recode(reg.partei, 
                                   'Keine Angabe' = "keine Angabe")) %>%
  mutate(wahl.2019 = recode(wahl.2019, 
                            'Da' = "Ja", 
                            'Nisam imao glasaÄko pravo' = "nicht wahlberechtigt",
                            'Ich war noch nicht wahlberechtigt' = "nicht wahlberechtigt", 
                            'Ne' = "Nein")) %>%
  mutate(partei = recode(partei, 
                         'Nisam izaÅ¡ao na izbore' = "nicht gewählt",
                         'Ich habe bei der letzten Bundestagswahl nicht gewÃ¤hlt' = "nicht gewählt",
                         'Ich habe bei der letzten Parlamentswahl nicht gewÃ¤hlt' = "nicht gewählt",
                         'Bez navoda' = "Keine Angabe")) %>%
  mutate(partei = recode(partei, 
                         'HDZ/HSLS' = "Regierungspartei",
                         'CDU/CSU' = "Regierungspartei",
                         'SPD' = "Regierungspartei",
                         'GERB' = "Regierungspartei",
                         'VP (Vereinigte Patrioten)' = "Regierungspartei",
                         'Die Linke' = "Opposition",
                         'RB (Reformblock)' = "Opposition",
                         'ÄŒitajte Bordigu' = "Opposition",
                         'Domovinski pokret Miroslava Å kore' = "Opposition",
                         'Die Regierung Bulgariens verhindert Briefwahlen.' = "Opposition",
                         'BÃ¼ndnis 90/ Die GrÃ¼nen' = "Opposition",
                         'Stimmen aufgeteilt: GrÃ¼ne, (spd) und DIE PARTEI' = "Opposition",
                         'BÃ¼ndnis 90/Die GrÃ¼nen' = "Opposition",
                         'FDP' = "Opposition",
                         'ĞĞ¸ĞºĞ¾Ñ' = "Opposition",
                         'Die Partei' = "Opposition",
                         'Nova Republika' = "Opposition",
                         'poÅ¡aram neÅ¡ bezveze' = "Opposition",
                         'PoniÅ¡tio listiÄ‡' = "Opposition",
                         'SRP' = "Opposition",
                         'BSP' = "Opposition",
                         'Domovinski pokret Miroslava Å kore ' = "Opposition",
                         'Da, Bulgaria' = "Opposition",
                         'Domovinski pokret Miroslava Å kore' = "Opposition",
                         'Koalicija â€MOÅ½EMO!â€œ' = "Opposition",
                         'Koalicija â€Pametno-Fokus-SIMPâ€œ' = "Opposition",
                         'MOST Nezavisnih lista' = "Opposition",
                         'Restart Koalicija' = "Opposition",
                         'Nisam izaÅ¡ao na izbore' = "nicht gewählt",
                         'Ich habe bei der letzten Bundestagswahl nicht gewÃ¤hlt' = "nicht gewählt",
                         'Ich habe bei der letzten Parlamentswahl nicht gewÃ¤hlt' = "nicht gewählt",
                         'Bez navoda' = "Keine Angabe")) %>%
  mutate_if(is.character, list(~na_if(.,""))) %>%
  mutate(reg.partei = fct_explicit_na(reg.partei, na_level = "Keine Angabe"))

#################################################################################################################################
## df Informierung-Quellen & Land --> reimport cleaned, pre-existing data from Excel)
info.quellen <- datensatz_alle %>% 
                       select(land, zeitung, medien, fernsehen, internet, radio, poli.akteuren, other)
info.quellen.2 <- read.csv("C:/Users/Matija/Downloads/Analysis.Boys/info.quellen.csv", sep = ",")

#################################################################################################################################
## df of all numeric variables --> mutated / separated in order to regress
ver.institut <- datensatz_alle %>%
  select(land, bezeug.btag, bezeug.partei, bezeug.kanzler, bezeug.prez, bezeug.justia, ver.mitb, ver.freund, ver.fam) %>%
  mutate(bezeug.btag = recode(bezeug.btag, 
                              '1 - sehr wenig' = "1",
                              '1 - u gotovo pa nikakvoj mjeri' = "1",
                              '10' = "10",
                              '10 - u velikoj mjeri' = "10",
                              '10 - sehr stark' = "10",
                              'Bez navoda' = "0",
                              'keine Angabe' = "0")) %>%
  mutate(bezeug.partei = recode(bezeug.partei, 
                                '1 - sehr wenig' = "1",
                                '1 - u gotovo pa nikakvoj mjeri' = "1",
                                '10' = "10",
                                '10 - u velikoj mjeri' = "10",
                                '10 - sehr stark' = "10",
                                'Bez navoda' = "0",
                                'keine Angabe' = "0")) %>%
  mutate(bezeug.kanzler = recode(bezeug.kanzler, 
                                 '1 - sehr wenig' = "1",
                                 '1 - u gotovo pa nikakvoj mjeri' = "1",
                                 '10' = "10",
                                 '10 - u velikoj mjeri' = "10",
                                 '10- Sehr stark' = "10",
                                 'Bez navoda' = "0",
                                 'keine Angabe' = "0")) %>%
  mutate(bezeug.prez = recode(bezeug.prez, 
                              '1 - sehr wenig' = "1",
                              '1 - u gotovo pa nikakvoj mjeri' = "1",
                              '10' = "10",
                              '10 - u velikoj mjeri' = "10",
                              '10 - sehr stark' = "10",
                              'Bez navoda' = "0",
                              'keine Angabe' = "0")) %>%
  mutate(bezeug.justia = recode(bezeug.justia, 
                                '1 - sehr wenig' = "1",
                                '1 - u gotovo pa nikakvoj mjeri' = "1",
                                '10' = "10",
                                '10 - u velikoj mjeri' = "10",
                                '10 - sehr stark' = "10",
                                'Bez navoda' = "0",
                                'keine Angabe' = "0")) %>%
  mutate(ver.freund = recode(ver.freund, 
                             '1 - sehr wenig' = "komplet niedrig",
                             '1 - u gotovo pa nikakvoj mjeri' = "komplet niedrig",
                             '10 - u velikoj mjeri' = "komplet hoch",
                             '10 - sehr stark' = "komplet hoch")) %>%
  mutate(ver.fam = recode(ver.fam, 
                          '1 - u gotovo pa nikakvoj mjeri' = "komplet niedrig",
                          '10 - sehr stark' = "komplet hoch",
                          '10 - u velikoj mjeri' = "komplet hoch")) %>%
  mutate(bezeug.btag = as.numeric(bezeug.btag)) %>%
  mutate(bezeug.partei = as.numeric(bezeug.partei)) %>%
  mutate(bezeug.kanzler = as.numeric(bezeug.kanzler)) %>%
  mutate(bezeug.prez = as.numeric(bezeug.prez)) %>%
  mutate(bezeug.justia = as.numeric(bezeug.justia)) %>%
  mutate(ver.mitb = as.numeric(ver.mitb)) %>%
  mutate(ver.freund = as.numeric(ver.freund)) %>%
  mutate(ver.fam = as.numeric(ver.fam)) 


################################################################################################################################
## Graphs
# Stacked bargraph for Info-Quellen
ggplot(info.quellen.2, aes(x = source, y = frequency, fill = land, label = frequency)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(position = position_dodge(width= 1), vjust= .5, hjust = -0.5) + 
  coord_flip() +
  xlab("Quellen") +
  ylab("Stimme") +
  ggtitle("Quellen der politischen Information")

# Violin-Box-Jitter-Stats for Zufriedenheit mit dem politischen System (zufr.system)
ggbetweenstats(data = datensatz_alle,
               x = land,
               y = zufr.system) +
  ggtitle("zufriedenheit mit dem politischen System") +
  xlab("Land") +
  ylab("Zufriedenheitsniveau")

# Bargraph for Mitgliedschaft einer politischen Partei (mitglied.partei)
ggplot(datensatz_alle, aes(x = land, fill = factor(mitglied.partei, levels = c("Ja", "Nein", "Keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(width= .9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("Ja", "Nein", "Keine Angabe")) +
  xlab("Angabe") +
  ylab("Anzahl") +
  ggtitle("Mitgliedschaft einer politischen Partei")
# --> w/ Percentages
ggplot(datensatz_alle, aes(x= mitglied.partei, group = 1)) + 
  geom_bar(aes(y = ..count..), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),
                 y= ..count.. ), stat= "count", vjust = -.5) +
  xlab("Angabe") +
  ylab("Anzahl") +
  ggtitle("Mitgliedschaft einer politischen Partei")+
  facet_grid(~land)

# Bargraph for Wahlregelmäßigkeit bei einer Partei (reg.partei)
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(reg.partei, levels = c("Ja", "Nein")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("Ja", "Nein", "keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Ob man die gewählte Partei regelmäßig wählt")

# Bargraph for vermutliche Wiederwahl einer Partei (next.wahl.partei)
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(next.wahl.partei, levels = c("Ja", "Nein")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("Ja", "Nein", "keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Ob man die gewählte Partei wieder wählen wird")

# Bargraphs for Parlamentswahl 2017/2020 (wahl.parlament) 
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(wahl.parlament, levels = c("Ja", "Nein", "nicht wahlberechtigt", "Keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("Ja", "Nein", "nicht wahlberechtigt", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Haben sie bei der Parlamentswahl in ihrem Land 2017/2020 gewählt?")
# --> w/ Percentages
ggplot(datensatz_alle, aes(x= wahl.parlament, group = 1)) + 
  geom_bar(aes(y = ..count..), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..count.. ), stat= "count", vjust = -.5) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Haben sie bei der Parlamentswahl in ihrem Land 2017/2020 gewählt?") +
  facet_grid(~land) 

# Bargraphs for Europawahl 2019 (wahl.2019) 
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(wahl.2019, levels = c("Ja", "Nein", "nicht wahlberechtigt")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("Ja", "Nein", "nicht wahlberechtigt", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Haben sie bei der Europawahl in 2019 gewählt?")
# --> w/ Percentages
ggplot(datensatz_alle, aes(x= wahl.2019, group = 1)) + 
  geom_bar(aes(y = ..count..), stat="count") +
  geom_text(aes( label = scales::percent(..prop..),y= ..count.. ), stat= "count", vjust = -.5) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Haben sie bei der Europawahl in 2019 gewählt?") +
  facet_grid(~land) 

# Bargraph for Vertrauen gegenüber Mitbürgern (ver.mitb)
datensatz_alle %>%
  mutate(ver.mitb = recode(ver.mitb, 
                           '1' = "komplet niedrig",
                           '2' = "sehr niedrig",
                           '3' = "sehr niedrig",
                           '4' = "eher niedrig", 
                           '5' = "eher niedrig", 
                           '6' = "eher hoch", 
                           '7' = "eher hoch", 
                           '8' = "sehr hoch",
                           '9' = "sehr hoch",
                           '10' = "komplet hoch")) %>%
  ggplot(., aes(x = factor(land), fill = factor(ver.mitb, levels = c("sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "Keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3, vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Vertrauen gegenüber Mitbürgern") 
  
# Bargraph for Vertrauen gegenüber Freunden (ver.freund)
datensatz_alle %>%
  mutate(ver.freund = recode(ver.freund, 
                             '1' = "komplet niedrig",
                             '1 - sehr wenig' = "komplet niedrig",
                             '1 - u gotovo pa nikakvoj mjeri' = "komplet niedrig",
                             '2' = "sehr niedrig",
                             '3' = "sehr niedrig",
                             '4' = "eher niedrig", 
                             '5' = "eher niedrig", 
                             '6' = "eher hoch", 
                             '7' = "eher hoch", 
                             '8' = "sehr hoch",
                             '9' = "sehr hoch",
                             '10' = "komplet hoch",
                             '10 - u velikoj mjeri' = "komplet hoch",
                             '10 - sehr stark' = "komplet hoch")) %>%
  ggplot(., aes(x = factor(land), fill = factor(ver.freund, levels =  c("komplet niedrig", "sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "komplet hoch", "Keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("komplet niedrig", "sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "komplet hoch", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Vertrauen gegenüber Freunden")

# Bargraph for Vertrauen gegenüber Familien (ver.fam)
datensatz_alle %>%
  mutate(ver.fam = recode(ver.fam, 
                          '1' = "komplet niedrig",
                          '1 - u gotovo pa nikakvoj mjeri' = "komplet niedrig",
                          '2' = "sehr niedrig",
                          '3' = "sehr niedrig",
                          '4' = "eher niedrig", 
                          '5' = "eher niedrig", 
                          '6' = "eher hoch", 
                          '7' = "eher hoch", 
                          '8' = "sehr hoch",
                          '9' = "sehr hoch",
                          '10' = "komplet hoch",
                          '10 - sehr stark' = "komplet hoch",
                          '10 - u velikoj mjeri' = "komplet hoch")) %>%
  ggplot(., aes(x = factor(land), fill = factor(ver.fam, levels = c("komplet niedrig", "sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "komplet hoch", "Keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("komplet niedrig", "sehr niedrig", "eher niedrig", "eher hoch", "sehr hoch", "komplet hoch", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Vertrauen gegenüber Familien")

#Bargraph for finanzielle Sicherheit 
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(finanz.sicher, levels = c("nicht abgesichert", "mittelmäßig", "ausreichend", "sehr gut", "student")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("nicht abgesichert", "mittelmäßig", "ausreichend", "sehr gut", "student", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Finanzielle Sicherheit")
view(datensatz_HR$finanz.sicher)

#Bargraph for Häufigkeit der Informierungsprozess
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(aktiv.tag, levels = c("weniger als 1", "weniger als 3", "weniger als 5", "mehr als 8", "keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Stunden der Woche", labels = c("weniger als 1", "weniger als 3", "weniger als 5", "mehr als 8", "keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Häufigkeit der Informierungsprozess")

#Bargraph for Wahrnehmung - Offenheit der Gesellschaft
# --> come back & coerce NA into Keine Angabe
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(off.gesell, levels = c("Ja", "Nein")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Angabe", labels = c("offen", "nicht offen", "keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Wahrnehmung - Offenheit der Gesellschaf")

#Bargraph for Vertrauen der Parteien
datensatz_alle %>%
  mutate(bezeug.partei = recode(bezeug.partei, 
                                '1 - sehr wenig' = "gar keins",
                                '1 - u gotovo pa nikakvoj mjeri' = "gar keins",
                                '2' = "eher keins",
                                '3' = "eher keins",
                                '4' = "etwas", 
                                '5' = "etwas", 
                                '6' = "etwas", 
                                '7' = "viel", 
                                '8' = "viel",
                                '9' = "viel",
                                '10' = "komplet",
                                '10 - u velikoj mjeri' = "komplet",
                                '10 - sehr stark' = "komplet",
                                'Bez navoda' = "keine Angabe"))
ggplot(., aes(x = factor(land), fill = factor(bezeug.partei, levels = c("gar keins", "eher keins", "etwas", "viel", "komplet", "keine Angabe")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Vertrauen", labels = c("gar keins", "eher keins", "etwas", "viel", "komplet", "keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Vertrauen der Parteien")

# Bargraphs for Gewählte Partei bei der letzten Parlamentswahl (partei) 
ggplot(datensatz_alle, aes(x = factor(land), fill = factor(partei, levels = c("Regierungspartei", "Opposition", "nicht gewählt")))) +
  geom_bar(position = position_dodge()) +
  geom_text(aes(label = ..count..), stat = "count", colour = "black", size = 3,
            vjust = 1.5, position = position_dodge(.9)) +
  scale_x_discrete(labels=c("Bulgarien", "Deutschland", "Kroatien")) +
  scale_fill_discrete(name = "Partei", labels = c("Regierungspartei", "Opposition", "nicht gewählt", "Keine Angabe")) +
  xlab("Land") +
  ylab("Anzahl") +
  ggtitle("Gewählte Partei bei der letzten Parlamentswahl (2017 / 2020)")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber dem Parlament (bezeug.btag)
ggbetweenstats(data = ver.institut,
               x = land,
               y = bezeug.btag) +
  ggtitle("Vertrauen gegenüber dem Parlament") +
  xlab("Land") +
  ylab("Vertrauensniveau")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber der Partei (bezeug.partei)
ggbetweenstats(data = ver.institut,
               x = land,
               y = bezeug.partei) +
  ggtitle("Vertrauen gegenüber der Partei") +
  xlab("Land") +
  ylab("Vertrauensniveau")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber dem Minister-Präsident / Kanzlerin (bezeug.kanzler)
ggbetweenstats(data = ver.institut,
                 x = land,
                 y = bezeug.kanzler) +
  ggtitle("Vertrauen gegenüber dem Minister-Präsident / Kanzlerin") +
  xlab("Land") +
  ylab("Vertrauensniveau")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber dem Präsident (bezeug.prez)
ggbetweenstats(data = ver.institut,
               x = land,
               y = bezeug.prez) +
  ggtitle("Vertrauen gegenüber dem Präsident") +
  xlab("Land") +
  ylab("Vertrauensniveau")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber dem Rechtssystem (bezeug.justia)
ggbetweenstats(data = ver.institut,
               x = land,
               y = bezeug.justia) +
  ggtitle("Vertrauen gegenüber dem Rechtssystem") +
  xlab("Land") +
  ylab("Vertrauensniveau")

# Violin-Box-Jitter-Stats for Vertrauen gegenüber Mitbürgern (ver.mitb)
ggbetweenstats(data = ver.institut,
               x = land,
               y = ver.mitb) +
  ggtitle("Vertrauen gegenüber Mitbürgern") +
  xlab("Land") +
  ylab("Vertrauensniveau")


####################################################################################################################################
## Multivariate regressions
# Multivariate regression for H6
h6 <- lm(ver.mitb ~ bezeug.btag + bezeug.partei + bezeug.kanzler + bezeug.prez + bezeug.justia, data = ver.institut)
summary(h6) 
Anova(h6)

# Multivariate regression for H8
h8 <- lm(cbind(ver.freund, ver.fam) ~ bezeug.btag + bezeug.partei + bezeug.kanzler + bezeug.prez + bezeug.justia, data = ver.institut)
summary(h8)
Anova(h8)








