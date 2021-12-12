import Data.List
import Data.Maybe

smolData = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

testData = 
  "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe\n\
  \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc\n\
  \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg\n\
  \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb\n\
  \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea\n\
  \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb\n\
  \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe\n\
  \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef\n\
  \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb\n\
  \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"

realData = 
  "fdb bgfa cedfg abedcf defgba gbaed dagfbce fbged dgabce fb | bf dfbega fb baged\n\
  \ce cgfdea acdfe befda afgdc fec gdfbca ebdcgf cgea adgcfeb | fgcadbe cefda aebdf degcfa\n\
  \egca gdcaeb cbdfe bdaegf dgbafec ge agdcfb degcb cgadb ebg | edfbc bcdeg bfcde baefdg\n\
  \dfbc bc facgd feagb bfadegc gacfbd acgfb bagedc gacdfe cbg | afdcegb bagfc egcafd bcg\n\
  \edc aedcbf dgcf cgebf egdacbf bdega egcbfa gcedb cd edgcbf | gdcebf bedfgc dbcgfe cd\n\
  \gcfabed fed facegd abdge gcdfb fbdge befa abgdfe dgbcae ef | bgdfe befa fgedb adcbeg\n\
  \fca fbdcega cadfg ac dbca dbcgf cgfaeb dbefcg gcfbad gafde | gfbacd fdgea gdfcb bdac\n\
  \eb bgafe bgcefd fegcadb daeb eafcg gbdaf bfe edgabf agfbcd | cfdebg aegbf dbagf dgbfea\n\
  \aebfgd gbcead dfbecga gdceb decfbg aeg ae gacfd eacb edgac | ea aeg ea dfaegb\n\
  \gdc fceagd cagf cgfdbe febcad gecad gc agdeb fcade bfaecgd | fdaecg gdc cegafd adebg\n\
  \bdgcef acd bdgacf gbcfd da agbcd bfda eadgcf bcgea cfbgade | fcbaged bfdegc gecdabf adbf\n\
  \bd faebcg cdgfa fcbda fbd fbcae aebd bgcdef gebcafd cdaebf | dbae fbd gcebfa gacfd\n\
  \gbaefc gbfdec gfdbe edbgcaf fdab dfgabe gadec agbde ba gab | bag fegdb badecfg bag\n\
  \feba fb fgbca acfdg gcfedb fdbecga eacgb abcgef fbc adgbce | gcfba afeb gbafc ebgfdc\n\
  \fegbc abdfce egdba cea cgabe faegdcb edfgcb cefbag afcg ca | dbgcfe gfac bgead aegbcf\n\
  \acdbf gfebc afe ea fdeagb fgbaced cfeadb eacd fgdcab bfaec | bgadfc bfaedc agcdbf ecgadbf\n\
  \cefag dbgecf cfbga dfceag gdfcbea dfebac eg cge gdae cadfe | dagebfc ge ge cbdfae\n\
  \cgefabd agbdc dgef bedfac gcf cafgd dfegac fg fadec afgebc | decabf febdacg bcdag fgc\n\
  \dfe bdfa gfeabd dacbegf agcfeb edacg efagd gedcfb bfgea fd | aedgc fd febag acgde\n\
  \dgcf efbcga adgfbc gbcfa adfebc eabgfdc dc acd dcabg egadb | fcdeba egcabf acfdgb dbgac\n\
  \cagfdb eagdc ea gacedfb beac bagdec gdfbae eag cgfed dacbg | dafgbe adegc daegc adcge\n\
  \adf ecgbda afegb dbacgf adfgb ecgfbda cgdf fd dacbfe cgdab | eadgbc adgbf gdaebc gdebcfa\n\
  \abfecd gdbaf cdfg bfd gacfbd df badegcf fgcba acebgf eabgd | abcfde edbag cfegab fdegbca\n\
  \cdfeba dbec dc dfcea bdfea cafebdg gadfcb cagfe fcd abgfed | gdabfc eacfg cabfde bdfgca\n\
  \eadcgb acefd degac dgface bcfaegd baegdf cfdg df ecbfa def | cefad dcgf fde feadc\n\
  \ceagbd bedgfac efbdc gd edgbc gbad gdc ebgca fgecba egcfad | ebgdca fcgade edgabc fagebc\n\
  \aedbcgf fg agfdc gbcad edfcgb egcdaf daecfb fgd ceafd afge | defac fg gedfcb gdcfeba\n\
  \egfbda dbcfeg gafc abdce dgfabec afged adegc cgdfea dgc cg | gcd degfa cbead bagefd\n\
  \de aedbf aed acfgde ebgaf cafdgeb cdfab defabg bedg acefbg | efdba befag dbafc fcaged\n\
  \cdgfba dfb efadbcg dfea ecfabg gdabef gbafe dcebg fegbd fd | fgaeb dfabge eabgf gdefb\n\
  \bagcef fabeg ebca fbged afe cdefag gcdbfa bfagc agdfbce ae | fdgbe cbgaf bace gbdef\n\
  \eafdg dfc cdfgbe bdce cebgfa gfceb dgcfe cd begafcd gdcfba | edbafgc ebgfc fcedg aefdg\n\
  \aegf bgedc cfbega fgcbda cfgeb gf eafcdb feabgdc fgc feabc | cedgb facebd bgadfc gefcb\n\
  \ebacgd faecb bd cdb cgaed bdeg fgbdca dcgafe eabcd afdbegc | dbc deacg dceba gfbacd\n\
  \aeb dcabge ba edacf bdaf fcgeb cfabde agfbdce afcbe aefdgc | abfd fgecad gdaecf afcgde\n\
  \dbgc gfcea cafgd fgcdab dg abdfc bcgeafd fgd eabfdc gdaefb | deabfg cgbd dcfbag cefag\n\
  \cabgfe baedcg cgd dg acfeg adgf bfecd fbegcad egcdf fgdace | cdg egcfba defgc acbedg\n\
  \ef fbcgd afde eafcgb bfgcdae dfacge egdabc eagdc gecfd cfe | caged efgdc gdeacf fe\n\
  \ae dfeabc fgea fbcegd fcdge ace cgdae bcefadg dbagc dcafeg | cgadb becafd dcbag fgae\n\
  \eafdbg fdgc fgeadc egacd egd ebcad bgfeca acefg fdgeabc gd | afebgd cfeadg gd ecbda\n\
  \ebgfc agcbd bgcdef ea gfea eafgbc cae ecbfad cegba egdbcfa | cafbde dgcfbe aebcfd badefc\n\
  \badfce cf ebgcd acgbfed afdgeb bfagec baedf cdbfe efc dcfa | debaf bgedfca facebgd ceabgf\n\
  \ebfac dba ecgbaf ad cbfdega edcbaf feda cebgda cdafb dcbfg | dacbf adb fgacdeb fdcba\n\
  \acdebgf gbdefa gaebd dgb gbfe aedcg aedfb afdebc gb cdagfb | geadb adfgceb eacgd debcaf\n\
  \adfb adfgce ecafd dgbace bd cfegb bfeadc begdcaf fedbc bdc | bd ecdabf dceaf dbc\n\
  \gaceb fg edbgca gafb fcbeg bfaegc eadcfg gcf bfdcega bcdfe | eacfgd becadfg gf fcbed\n\
  \bdfcg ad begac gfbadc dga abcgd dcaf gcdaefb gcbdfe gefdba | dfbcg cdabg adcbg dagbc\n\
  \febgdac fgadce fa gdfbc dcgfba gcfebd afc gabcf bagec bdfa | agbec bdfa gcbaf fbcgde\n\
  \bdfgae fgead faebgc dabg bcfdge gde gefba gd edcbafg dcfae | cgfaeb fgcabe cafgeb gdcbfe\n\
  \eadcbf agfdb bfa ab dfgbc faedg aegb dcfaebg gbfaed dfcaeg | fagbd dfgab fgadb adfceg\n\
  \gaedc aegdcbf bdgf feagbc cedfg dbfec ecdbgf fge fg cbdaef | agedc dbfegc fg efcdg\n\
  \db becfag abd agebd bgcae bced efdag gbcade gbcadf gcbeafd | bd bdgefca dgeabc bd\n\
  \febag deg gefdab becdag defa dbfge gbcfd gcbeaf bfdgaec de | fead bagef abfeg aefd\n\
  \dfac aegcbfd aefcg daebgc eadcg fa bfeadg adgcef fae fgceb | cdeagfb fea cedgab bfcge\n\
  \ed dbfgce efdcb bafcged dcge bcadf afecbg dfeagb fegbc edf | debgaf ebcdf de fdgbec\n\
  \fadg dgefc da eafcd adc gdecbf bcgdae afecdbg dgafec caefb | caedf eagbcd gdaf gefdbca\n\
  \degbcf gfdbace fbced bedg abgdfc gecfa fceadb ecbfg fgb gb | gb gbdfca dceabf fcegb\n\
  \bagfce gfbac fbdcga ceab edagf eb afgeb gfdbec cfdbgea gbe | fdgceb fdage fgbac bfeag\n\
  \afbecg aecdg bfcd dfecgb febgad efdcg fed aedgfbc df efbgc | dfcb dfcb cbgef fd\n\
  \defca bgac egdfbc gfbea fgabde ceagdbf bfaecg cb afebc fcb | cb acfeb ecabf ebfca\n\
  \cdfbga befa daebc afcdbe fdabceg aed gdcbe acfdge cafbd ea | dae cedbaf dcgeb afbe\n\
  \dbefg gecad ecafgd aedgcb fecdba cb cbedg cbd ceabgfd bacg | fegdac eadgcf geabdc cedag\n\
  \abdcfg eagf cgf ecgbd badfce cabgefd gbcef aecfb fcabeg gf | gf gabfdc cgebf abcfed\n\
  \agdceb ebadc dfebc gbdac fegdac adbfgc cae ea befdgca ageb | edfbc acbefdg cdgefa bdcga\n\
  \efca fgbca gabfe eab ae cfaegb caebdg abfgcd dfgbe cadbgef | bfacge fabcgd eafbgc cfgbad\n\
  \cedbgf efbagcd aegdcb dabef feb ef badgf cdeba acfe ecdfab | ef eadfb baedcg edfacb\n\
  \af gfa egbdf ecgabd afcgdb fbadg facegd cbaf gcbda eacbdgf | abdcg gfadbec bfedg cdbga\n\
  \dgbea afeg ef cbedfg cbgdea fcabd adfbe efd efdgba gecfbda | cfbaged ecbagd fbgecd ebgda\n\
  \gcbeaf bf ebadfg dafb bcgde daegf cagfde bdacefg bfe dgebf | gceadf gefacb fgdeba gfbdea\n\
  \dgafbe dgfce cb cbafde eadbf dbca cbf cedfb gfaceb fdegbca | bedacgf bfc cdab ecbagf\n\
  \cadbef dfgac gfb bcedf gbdafe cgbe gdcfb cagbdef egfbcd bg | gdfbc ecbfdga bdcgf bacgfed\n\
  \dfcb cd gdbae gabdfce bfceg bdcefg ced cfdeag ebdcg efgcba | fgecba edbgcf cbgfed fgdeac\n\
  \ebgdcf cdfa cgafe ace gfedc cadfge aebgf adgceb fdecgab ac | cegdf gfeba edcfg ac\n\
  \agbefd bdgfe fdcegb dce ebgcd bcdga decbafg ec eacfgd cbfe | ec edgbc cgdba ec\n\
  \bgadc adbecfg gdbfce fdg dfeac agfb fdgac fg cbfgad dcgeba | gf dcfbge bgadc fg\n\
  \badegf abegc dfgc gbdaf fdgaebc cgbfa cf dcbaef cdbgaf bfc | dabgf cageb adfgb bdfagc\n\
  \ce abgdef ecdg fgbca fdage cadgef eca acfge acfbde bedfgca | cfage bagfc gbcfa eac\n\
  \gdfbe gcfeb febgda cfb fedc baceg ecgdfab dagbfc cf fbcdge | gfebc cgdefb bcf gebca\n\
  \gceabd bc ecb bdcef ebfacd fgeadcb abfc dgfec bdafge fdeba | gcbefda egcdba ecbgad gfbead\n\
  \gabec gaefbcd bgeaf ebf dbfag ef edgafb fbdgec fade bdfgac | bef abdgf fe ceagb\n\
  \bcfaged fdbgc dafcg edfacg dfb fb bdagef cfba dgbec gfdacb | fadgc cgbfad dcbeg cafb\n\
  \bdg bfdcae eagdb eacgd fagb afebd ebfcdg fcaegbd bg egdafb | fbedgc ebadf dgeabf aefbdc\n\
  \ad cgadbef eabcfd fdabgc dba dbace ecdbf fdae bdfecg acegb | bcgfda gfdcbe eafcbd cbgea\n\
  \efbda dcbfe bc bcfg egfcd gdcefb agbcde bec ecfadbg caefdg | dcagebf bc bfcdaeg cabfedg\n\
  \agde gdacef adgfc cbdfaeg cgfde cde badfec abcfdg fbcge ed | cde eafbdc cdfbae afbecd\n\
  \fdg bgfeac afgcbd bcfag fgeacd cdfb gbead fgdba cgfdaeb fd | bgacf gecdfa fd dfg\n\
  \ga agc cedgbfa bcfdea fdgcb geab gfbca ceabf edafgc cgefba | bgcfa fcagde edbgafc becaf\n\
  \agceb ecfgba decgaf eaf ef cdbaf gdcafbe fgeb gcaebd ecabf | fbdcage aegbdfc bagce fdagce\n\
  \cdage aebfdc bgacf fbeagc cfd bgfd defcgab acfgd gafbcd fd | cfadg dbface dgbf gdfac\n\
  \cefbda afcdb dgecfb dab cdefb bafdge ad deca gaecdfb acfbg | cbdgfe fecdb bdfce efbgdc\n\
  \aefbd ebcgfa ad ade egfbd abcefd cfeab bacd afedcg dbcagfe | edcbaf gfedcab dfgeb gefbd\n\
  \agdb ecfgb fcbaed agfdc bcd fgdcba faegbdc deacfg db dgcfb | dcb gcedfa agdfc bdacgfe\n\
  \gdfeab gcdfeb dg fdebc bdacef fbcegad gbced cdgf edg baecg | gcdf dg gbeac bedcf\n\
  \eb degbc bead cbegfa cdgfb gecad gbacde bce adfecg eafbgdc | dfaceg dbgce gcadfeb bcgdea\n\
  \dgcefb cad gcbad da dfcgb bcega bcfgda afdg dbfceag fcdeab | egabc bgfdce gcadb fbacgd\n\
  \gfadce eabfgc df gedab cdgf debcfa eagdbcf acgfe edfag afd | fadeg cbeadf aecgbf adfeg\n\
  \abcdg fecd ged aegcfd dgeca afegcb afcge debgfa de eafgcbd | ged efcgda gefcad aegfc\n\
  \gbfecd gafcdeb ebcgd efgbad fg dfgbc egcf egbdca fadbc fdg | dbacf gf fbdecg gdceabf\n\
  \ebdf dbafc gdcabf dagecbf cabedf afgbce ef cfaed gecad aef | ef dacbf febd gcbafe\n\
  \daebfg gfceadb agbde aegbc ebagcd cdgb cg baefc egc dgafce | cadbeg agbdec eafcb gebac\n\
  \dcfgbe ca acgde begdc dabc efdag aecdgbf ceadbg cae aecbfg | cgebd bfedgc defga bdac\n\
  \cbdf dacge fadec df dbafge eafcb fbdace dfa bgacfe cgafdeb | df eadcfb cdefa fecad\n\
  \fb feb cgeab cagdbe ebfadc gbcf adfge aebfgc acgbdef bfgea | efcadb gdcbae fbe afegd\n\
  \dgcf gaecb gfdeba bcdef dgb fgadcbe dg fdebca dcegb begfdc | bdg ceabg gabce gd\n\
  \dagc gaedb dgeabc ag ebcdg age fcaebg adfbe cdefagb gbcdfe | bdceg gecabfd acdfegb gacfdeb\n\
  \acefdg gacebd fdga bgdcfae afceg ecadf fgebc cdfbae gea ag | cfagde ega dcefab afdg\n\
  \bcgadf defgb cfea ec gdeacb efadcbg ecd ecgadf edcgf fdcag | dce fdgbe egacbfd efcagd\n\
  \dcagb dagbcef aedcgf deafb adbfg bfgc bgafdc gcedab gfa gf | gdfba fedba eafdcbg fg\n\
  \fedacb gdbcae gefda fbcae gecafb afegc gca gc edfbgac fbcg | bgcf gac ecafgb egadf\n\
  \ecfgb fdbega cbga gce bacfeg agfedc cbfgaed cefdb bgefa gc | dgacfe daefgcb gebfa ecg\n\
  \gefadb dcfbge ag abg fbcad cfgbea edga dbegf dgfba egbcdaf | ebfdga adgcebf gbdecf dbgfeca\n\
  \ecfgbd agde eacfgdb dgcabe edbcg facbd cabfeg bdace ea aeb | aeb fabdc egdcb ebacd\n\
  \ecgfda af fac cgfde cgefbd fagecb daceb gafd ebfadcg efadc | af fcegd bgceaf fca\n\
  \ca cgade adefgb dgfea edcbg dgeafc cfga dgbceaf dfaebc dca | cedga bdceg degfa ca\n\
  \gfced dgecfb ga ceafbdg afedb acdg gfeadc cebgaf fag fdgae | cgfeab ga dcfeg dbafe\n\
  \bcefda fcega dbga abgcdfe bgfdc dfgbce dca ad fgdac cadgbf | ad bacdgf da gcdeafb\n\
  \gdcea caebgdf agfdeb cfabge cbdafg gef bcef acgfb ef fagec | ecgaf fcage acged fe\n\
  \egc bdfge cedbg befc dabgecf dgfeac cgdab ce dcfebg bgfdea | dcgeaf ec egbfd ec\n\
  \gcbdaef fd dbecag fde gcdf acebf fgdbea gbdcfe dcgbe cefbd | egfdab fd cgfabed cbdegf\n\
  \abec baf ecgaf facgbe fecbdga ba fagecd bgcfda bfage dfbge | bgceadf ebdgf dfabgc acegfd\n\
  \dca eafc dafcgb cbead efabd cfbade faedbgc gdecb gfbdae ac | adefb adfecb cfbeda acfe\n\
  \eac becfgd fgadcbe geafd fcade eagbfc bcad ca abedcf fcbed | fcbed bacd fdgecb abcgfe\n\
  \bfad cbged efdgbac gdefac fd acbgf bdfgc cafbdg acgebf cdf | fdcgae gabdcf gcbdaf cabfg\n\
  \ebg bdfce gfecb dgceafb cafeg dgfcea cafegb gb cgadbe bgaf | cebgaf dcebag gbfa egacdfb\n\
  \edbacf eba dbgcfea dbef abdcge bcgfad gcaef be ebfca cdafb | gbedca cbafd edfcab bdfe\n\
  \fcdbae efacd aebd gbedfc afgdc gfcdeba aef efdbc ae cfbeag | bcfde abcgfe dcagebf gfbdce\n\
  \abg gcdfb ag afegdbc bfagd bcdefa fedgab eagbcf deag adebf | ga bdgfc efabd edbagcf\n\
  \fb befc egafcd gdbcf gecbafd fgb dgfce dcgba cdbgfe adebgf | cbgfde bf bdfceg edgbfa\n\
  \gefab efdabcg eagcbf de eabdfg geda egcfdb faebd cadbf edb | aefcgb fbade de afbge\n\
  \edfcgb ecadgb adbcf decbf fcgaed dfegc ebd dbagcef egfb be | gbef fgeb eacdgf dfacb\n\
  \dfc ecfgad abcfd befgca cgfabd cagbf gbdc dcbaefg dc bdefa | cd ebafd dc fdc\n\
  \aefgc cfdga acbefd gebdacf efg cbefga fceba eg begfdc bega | gbfdce dfbegc geba dagcf\n\
  \cfgbad fadebgc cga efgcd afecgb ac acegf ebgfa fbgeda ebac | fgbedac ac ecba beca\n\
  \dc gbcfda dbc egabc gcadbef agbcde bgdef deac bdecg gecafb | ecad gfbaec dc cegbd\n\
  \cadgef eb cgdeab ceb cbgdf befgc egcbfa cefdgab fbae cfaeg | cgdbea acbefg gaefdc gbecf\n\
  \cdfbea gbceda cgdfb cdbge geb acgefb dage eg afcgedb beacd | aebdcg gbdeafc ebdcga bcged\n\
  \cgbfed ba agefdb acbedgf adegb dcega abdf abg gfbde gbcefa | bgedf dbgfe dfab edbfcg\n\
  \ga geabc bfedacg efga bfgaec bgecf agb cdbfag abdec fcbegd | egfdbc cefabg ga gaebc\n\
  \cfebg cfgbea acdgf edgbfc dgfabec db fdebac bedg dgfcb dbf | dcebfa egbd dfb fdcgeb\n\
  \ecdgf ad ebdgac dca caebgf gebacfd fbace dabf bdecfa fecda | bagfecd dca abgcde bcefga\n\
  \de def dbeafc dcbaf efacdgb cfagdb afcegd ecdb gfeba dabfe | bdec fadeb de febag\n\
  \bdgfac ec cde fegc fdbeca fcgdb cedgfb gbecd fecbgda edagb | fcgabd ecfg bfadgc ec\n\
  \gfc fcbagd fg dfeg gbeadcf becaf bcgeda cgaed acgfed ecgaf | eadcgb cbfea gdef ceagf\n\
  \degac bcegd cedagb ebc agfbedc dfcgb adeb be cgadfe agcebf | eb daeb cgbed fcgdb\n\
  \aebcfd feabgc fgbdca bfcag df fagbd fdagceb fcdg baedg fad | debag dcfbea bgdfa gbacfd\n\
  \bef fe agebd fgacbe dcfba aebdf edfg gecdab fgcbdea afdebg | fdeba dbeaf bdceag adgbef\n\
  \cdefb gcabdf cgbdefa eg ebg fbegd bafgce bdaegf dabgf dgae | geb bfgacd gdea abgfdc\n\
  \acegdb fcge gceabf fadbe fgcba eagfdbc ce ceafb cafdbg ace | bfead dfcbgea faebd dfaeb\n\
  \agbfe bcfgdae bca gcfb gefabd bdecfa deagc cb egfcab begac | becag gedac bfgc caedbf\n\
  \adgef egcd gda beafd gcfbda cefdag gd dgcfeba egbcfa aefcg | abgdcef ebdfa edgc eafdb\n\
  \adecf faebd gaefcb ebf bfdc badge efadbc bcfeagd agcedf bf | dgaeb edcfag fb cbegfa\n\
  \gcfa becda dabgfe af ecfda efgadbc gcfaed egfcd fcgbde eaf | fae fecgd adfebg afedcg\n\
  \gbacdfe gfaecb fgeab cg fecdbg bcafg aecg abcdf fgadeb cgf | ebcfag cbdgfe bgfecd gecfbd\n\
  \adgcef gfbde dafcgb bceagd edbgc dbc baec bc eadfbgc ecgda | cb gdbeca facdeg fdbge\n\
  \fgabe ad dfbce fad facebg fbeadg fcgaed ebafd bgad gfceadb | dcfeb egdbaf fbecd efcdb\n\
  \bgfdc gbacef fgdeba abefg afec bcgfe aebfdgc ceg degacb ec | ec fegab fgcbe ec\n\
  \egfdc cbagdef fcaed fdg dcefbg dbcge fg dcbega bcfgad befg | cdeaf dgf abcgdf edfcg\n\
  \cdfgaeb eagcd decf efdcga ed fagdc gabec egd bfedag bdgacf | cdef eabcg acegdf egbac\n\
  \badfecg gdfabc bd dba acgbd fbdaec gbdf agbfc gecda fbcega | dbgac bcgfa ebgacf gcdea\n\
  \bacgfd dabec ebdgc fgcbade fdeg fbgdec dg begcf dcg cafgbe | gaefbdc dcbae fedbgc acebd\n\
  \bdcgf aedbgc befa gcfeb fgebac efagc eb efagcd efdgbac egb | gfabce acfged egdafc ecfag\n\
  \fade facbged defcb fe gdbace fgbcd cagbef bceafd feb edcba | ef cdbea fbe becdf\n\
  \edcag gaefd edcf cfegad egdbafc cfaegb aec bdcag ce gbefad | bdfgae abcgd cae cdbgaef\n\
  \cegbf ebfca gbc gc debagf cdgbef becagdf bfdge dabceg gcdf | ebfca cegbf ecbaf eafbc\n\
  \dgafe gbcdef efcdab bcga eafcdgb cefgba abe gcefb ba egbaf | gaefbc cfagbe fcbeg cedgbf\n\
  \befgac bed dfcabe db fcgeb gfeda decafbg egcbdf bfdge cbdg | eafdg agfceb febagc gbefd\n\
  \cdbfgae gcdfea agb gbfad dgfcb faegd ebda eabgfc ba bdgaef | gdfab cbaefg bdgaf fbgeac\n\
  \cabgd cbfagd fd gecbda fceadgb fgadbe fgdc ebcfa cbdfa dfa | gefabd fd cadfb gcbdea\n\
  \efga gfbce abgce cbgead cagbdf egdfcab bafecg bfecd fg gbf | fcebd geacb gadfcb fbcgae\n\
  \efg eagfd gcea bafgecd gdfebc bdaef cdafgb afgdec dgafc eg | eafdb dafbcg cadfg caeg\n\
  \ebcgad dbe efcgd caegb cgbefda dagb fabdce ebcdg cebagf bd | cfbaeg defgc fgced becfga\n\
  \bcadge fbedg aegc dbfcag ebgad gdbac gcfdeba ae eba bcdafe | gace agcefdb gbade eba\n\
  \eabd bfcagde egacd cgebfd ad dcgeba aecfg ebcdg cfdagb acd | dcgbaf cafeg egfac bfcged\n\
  \ebaf gcdeabf edgba dfacge gfbde fgdcb aecbgd bedgaf edf ef | bdegf ef adgbef dgface\n\
  \fbedca dgefa adebcgf bacfg efc fgeca fagced ce degc gaebdf | gcfea cfgeda ebfdcga ec\n\
  \ecba fedcabg bgedf age dcfgae feacgb ae agcfb ebagf cadbgf | fegba efagb afecdg afebg\n\
  \ea edbgf efadgc bdacfeg bgafe bcafg deba edbcfg debfga fea | fgedba bade gacbf adbe\n\
  \fdbage cabgfe edb efabg aegd gdecbf fbdae de fabedcg bdcaf | begdfc adefb gfdbce aged\n\
  \dafcbge febadg cfdea fdgcae ga gaf gcda bfcade egcbf cgeaf | ceagf fcegb afg adefc\n\
  \adfebc dfgb bg befacg daebf bfegda daegc bdgae gfacbde bag | aedcg gab abgde debag\n\
  \daegfb dgec aedcfbg fdcgeb bdcef fcadeb beg ge bgfce bcagf | fcgbed dbfega beg febdc\n\
  \fgcead edgbcf adebf cfb bfacge bcfae cfeag bagc gfcaebd bc | efbad abgc edgabcf acbg\n\
  \bcafegd eafdg dbfgc edgfc cdgfab geabdc gce ec egfbcd fbce | facbdg badgce dfegc aegfd\n\
  \bcdafge bedfag cfgbad abgc dgabf dgfcb cb cdb dcefg fabdec | dcb bdfagce cbfgd cdb\n\
  \afebdc adebc gdbca fbeadcg bacfeg eafd de bcdegf edc abefc | ed cgdab feda efacb\n\
  \agecfd bgcefd fecba ga egad agbfdc fcaebgd feacg gfedc fga | dega caefb fcgea fcaeb\n\
  \bfcegad facbgd egbfa ca gcafbe afec cdebg fbeadg bca acgeb | bfgace acgfdb cgedb beafg\n\
  \cefbad fd befd ecabfg gefacbd bcefa fdeca fad dgaec dbfgca | adf geadc fd dfaec\n\
  \adfbe becagf geabf ge gacbf efg gfdcea cfbdag ecfbadg becg | fgbae afdcgeb eabdf cgbaf\n\
  \fgdcae bgdfae cfdag ea egcfa gcbfe becfgda aef ceda dabgfc | gcfaed acdgbef efbgc cade\n\
  \cgbfde cdb aefcdgb cegda dgbca bafdg fbac badcfg cb gfadbe | dcb bacf cdegfb bfdag\n\
  \gdcbe bcdega geadc bfgdae fgcad eabc cgefdb gae acfbedg ea | gbdec gcdea befgdc gecda\n\
  \df abcgdf gfeba bfagdec gbeacd bcaefd abdfg abdcg fdb dcgf | fd fdcg abgdfc gbaedfc\n\
  \eg eadcf dfabeg cedg aecdfg afgcb cefdba debfcag egf aegfc | fecad fcagbed ecfag cagfb\n\
  \fbeadc gfa fg fagbd gfdebac eafdgb dacbg fcbeag fedab gefd | agedfb fg eafcdb bgedaf\n\
  \gbcadf begcfd fcbed caed dafeb eacdbf faebg cdeafgb abd ad | cbefd cfebd cead dab\n\
  \dceba gbcaed ag gdcbf geab bdaegfc acg acdbg bcfaed afcdeg | gcdbf edbca agcbd cdgba\n\
  \facbg fdgbca afc fgcade fegbacd af cgbad cgdeba fbda cefgb | fac caf fca cabfg\n\
  \badecf acbdf efac agedfb bgedc aeb baedc bgcdfa ea fgbcead | face eafc bacfd gacbdf\n\
  \bc ebfgcd gafbcd dgeacf gfbea bgc dcbe faebgdc ecfgb ecfgd | edbafgc bgefa bcfgde dfcage"


type Input = String 
type Output = String
type Permutation = String

parseData :: String -> [([Input], [Output])]
parseData = map (break (== "|") . words) . lines

getOutputs :: [([Input], [Output])] -> [[Output]]
getOutputs = map tail . map snd

getInputs :: [([Input], [Output])] -> [[Input]]
getInputs = map fst

zipData :: String -> [([Input], [Output])]
zipData d = zip (getInputs $ parseData d) (getOutputs $ parseData d)

outputString :: ([Input], [Output]) -> String 
outputString (i, o) = map (decode (head (getPermutationsForInputs i puzzlePermutations))) o

firstPartAnswer :: Int
firstPartAnswer = length $ filter (\o -> (length o == 2) || (length o == 3) || (length o == 4) || (length o == 7)) $ concat $ getOutputs $ parseData realData

puzzlePermutations :: [Permutation]
puzzlePermutations = permutations ['a'..'g']

validPermutations :: [Permutation] -> Input -> [Permutation]
validPermutations [p] _ = [p]
validPermutations ps input = filter (\p -> isDigit p input) ps

getPermutationsForInputs :: [Input] -> [Permutation] -> [Permutation]
getPermutationsForInputs _ [] = []
getPermutationsForInputs _ [p] = [p]
getPermutationsForInputs [] ps = ps
getPermutationsForInputs (i:is) ps = getPermutationsForInputs is (validPermutations ps i)

smolDataPermutations = concat $ map (\i -> getPermutationsForInputs i puzzlePermutations) (getInputs $ parseData smolData )
testDataPermutations = concat $map (\i -> getPermutationsForInputs i puzzlePermutations) (getInputs $ parseData testData )
realDataPermutations = concat $map (\i -> getPermutationsForInputs i puzzlePermutations) (getInputs $ parseData realData )

decode :: Permutation -> Input -> Char
decode p i = 
  if isZero   p i then '0' else 
  if isOne    p i then '1' else
  if isTwo    p i then '2' else 
  if isThree  p i then '3' else
  if isFour   p i then '4' else 
  if isFive   p i then '5' else
  if isSix    p i then '6' else 
  if isSeven  p i then '7' else
  if isEight  p i then '8' else 
  if isNine   p i then '9' else 'Q';

isZero' :: Permutation -> Input -> Bool 
isZero' perm input =
  if (length input) /= 6 then False else
  if   (isJust (findIndex (== (perm !! 0)) input))
    && (isJust (findIndex (== (perm !! 1)) input))
    && (isJust (findIndex (== (perm !! 2)) input))
    && (isJust (findIndex (== (perm !! 4)) input))
    && (isJust (findIndex (== (perm !! 5)) input))
    && (isJust (findIndex (== (perm !! 6)) input)) then True else False

isZero :: Permutation -> Input -> Bool 
isZero p i = (length i == 6) && generatePermChecker [0,1,2,4,5,6] p i

isOne :: Permutation -> Input -> Bool 
isOne p i = (length i == 2) && generatePermChecker [2,5] p i

isTwo :: Permutation -> Input -> Bool 
isTwo p i = (length i == 5) && generatePermChecker [0,2,3,4,6] p i

isThree :: Permutation -> Input -> Bool 
isThree p i = (length i == 5) && generatePermChecker [0,2,3,5,6] p i

isFour :: Permutation -> Input -> Bool 
isFour p i = (length i == 4) && generatePermChecker [1,2,3,5] p i

isFive :: Permutation -> Input -> Bool 
isFive p i = (length i == 5) && generatePermChecker [0,1,3,5,6] p i

isSix :: Permutation -> Input -> Bool 
isSix p i = (length i == 6) && generatePermChecker [0,1,3,4,5,6] p i

isSeven :: Permutation -> Input -> Bool 
isSeven p i = (length i == 3) && generatePermChecker [0,2,5] p i

isEight :: Permutation -> Input -> Bool 
isEight p i = (length i == 7)-- && generatePermChecker [0,1,2,3,4,5,6] p i

isNine :: Permutation -> Input -> Bool 
isNine p i = (length i == 6) && generatePermChecker [0,1,2,3,5,6] p i

isDigit :: Permutation -> Input -> Bool 
isDigit p i = (isZero p i) || (isOne p i) || (isTwo p i) || (isThree p i) || (isFour p i) || (isFive p i) || (isSix p i) || (isSeven p i) || (isEight p i) || (isNine p i)

generatePermChecker :: [Int] -> Permutation -> Input -> Bool
generatePermChecker [] _ _ = True
generatePermChecker (d:ds) p i = (findPredicate d p i) && (generatePermChecker ds p i)

findPredicate :: Int -> (Permutation -> Input -> Bool)
findPredicate index = (\p i -> isJust (findIndex (== (p !! index)) i))