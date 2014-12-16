Sähköinen kilpailutulospalvelu
==============================

Yleiskuvaus
-----------

Verkkosovellus frisbeegolfkilpailuiden tulosten kirjaamiseen, arkistointiin ja katselmointiin. Tulosten syöttäminen tapahtuu yleensä älypuhelimilla, mutta sovellus on toteutettu siten, että sen käyttö onnistuu kaikilla alustoilla. Käyttöliittymä on kuitenkin suunniteltu toimimaan pienellä näytöllä.

Sovelluksella on muutamia erilaisia käyttötapauksia. Kaikki pelaajat voivat käyttää sitä omilla laitteillaan omien tuloksiensa kirjaamiseen, yksi pelaaja voi kirjata useiden pelaajien tuloksia, erillinen toimitsija voi kirjata muiden pelaajien tulokset tai mitä tilanne sitten vaatikaan. Yksittäinen pelaaja voi myös käyttää sovellusta omien tuloksiensa kirjaamiseen esimerkiksi harjoituskierroksella.

Käyttäjät voivat tehdä sovellukseen oman tunnuksen, mutta se ei ole pakollista. Tunnuksen tarkoitus on helpottaa sovelluksen käyttämistä, mutta se ei tarjoa lisäominaisuuksia käyttäjälle.

Tekniikat
---------

### Haskell
Puhdas funktionaalinen ohjelmointikieli. Päädyin valitsemaan Haskellin ohjelmointikieleksi, koska se on turvallisuuden kannalta erinomainen valinta. Käännettävyys, staattinen ja vahva tyypitus tarjoavat hyvän pohjan lähteä rakentamaan turvallista ohjelmaa.

### Yesod
Web-ohjelmistokehys Haskell ohjelmointikielelle. Yesod tarjoaa esimerkiksi tyyppiturvalliset url-osoitteet ja tukea XSS- ja CSRF-hyökkäyksiä vastaan. Valitsin Yesodin alustaksi, koska se vaikutti ominaisuuksiltaan parhaimmailta alustalta. Useita kirjastoa ja ominaisuuksia valmiina kuten sessioiden hallinta, sähköposti autentikointi ja eri tietokannat.

### SQLite
Sovellus käyttää tiedon tallentamiseen SQLite-tietokantaa. Sovellusta ei ole kuitenkaan sidottu kyseiseen tietokantaan ja sen vaihtaminen tarvittaessa on helppoa.

### jQuery
JavaScript-kirjasto HTML-dokumentin muokkaamiseen, tapahtumine käsittelyyn, animointiin ja Ajax:iin (Asynchronous JavaScript and XML). Useat sovelluksen osat käyttävät jQuery kirjastoa asiakaspäässä mahdollisimman hyvän käyttettävyyden saavuttamiseen. Esimerkiksi tulosten syöttäminen.

### jQuery Mobile
JavaScript-kirjasto responsiivisten nettisivujen tekemiseen, jotka ovat käytettäviä kaikilla laitteilla. Käyttöliittymä on toteutettu kokonaan käyttäen jQuery Mobile kirjastoa.


Ominaisuudet
------------

Kaikkia ominaisuuksia ei ole vielä toteutettu tai tämän hetkinen toteutus on ristiriitainen dokumentaation kanssa.

### Käyttäjä
Jokaiselle pelaajalla luodaan käyttäjätunnus, mutta sen aktivointi on vapaaehtoista ja käyttäjän vastuulle. Aktivointi tehdään sähköpostitse. Käyttäjä antaa oman sähköpostinsa, jonne sovellus lähettää aktivointilinkin. Käyttäjä voi sitten seurata linkkiä ja aktivoida oman tunnuksensa.

### Kilpailu
Kilpailu koostuu layoutista, päivästä, nimestä, pelaajamäärästä ja tilasta. Ylläpitäjä luo sovellukseen uuden kilpailun johon käyttäjät joko ilmoittautuvat itse tai ylläpitäjä lisää heidät manuaalisesti. Ylläpitäjä voi sitten hallita kilpailuaan haluamallaan tavalla. Pelaajien poistaminen, kilpailun aloittaminen, seuraavan kierroksen aloittaminen, ryhmien järjestely jne.

### Rata, layout ja väylä
Rata koostuu aina vähintään yhdestä layoutista. Ylläpitäjä voi lisätä uusia ratoja ja niille uusia layoutteja sekä muokata vanhoja. Rata koostuu vain nimestä. Layouttia sidotaan johonkin rataan ja silleannetaan nimi ja kuvaus. Jokaiseen layouttiin sidotaan rajaton määrä väyliä joille määritellään numero ja ihannetulos (par).

### Ilmoittautuminen
Kun kilpailu luodaan, sille tehdään automaattisesti ilmoittautumis-sivu, jonka kautta käyttäjät voivat ilmoittautua kilpailuun.

### Käyttäjien hallinta
Pääylläpitäjä voi katsella ja muokata käyttäjien tietoja sekä luoda tavallisia ylläpitäjä.

### Sarja
Useita erillisiä kilpailuja voi kytkeä toisiinsa määrittelemällä ne kuulumaan samaan sarjaan.

### Tasoitukset
Ylläpitäjä voi määritella haluaako hän, että kilpailulle lasketaan tasoitetut tulokset. Tasoitettu tulos tarkoittaa tulosta, jossa pelaajan aikaisemmat tulokset vaikuttavat hänen lopulliseen tulokseensa.

### Käyttäjien tunnistaminen väliaikaiselle salasanalla
Käyttäjät, jotka eivät ole kirjautuneet sisään omalla tunnukselleen tunnistetaan väliaikaisen salasanan avulla. Sovellus generoi salasanan ja ylläpitäjä kertoo sen käyttäjille eli pelaajille. Väliaikaisen salasanan käyttäminen voidaan myös pakottaa, vaikka käyttäjä olisikin kirjautunut.

### Tulosten pitäminen muistissa, jos verkkoyhteys on poikki
Tilanteessa, jossa laitteella ei ole verkkoyhteyttä, sovellus pitää syötettyjä tuloksia laitteen muistissa ja lähettää ne palvelimelle välittömästi yhteyden palautuessa.

