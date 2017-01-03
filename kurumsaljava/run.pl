$sessiz = "qwrtyplkjhgfdszxcvbnmçþðQWRTYPLKJHGFDSZXCVBNMÇÞÐ";
$sesli = "âýoueaöiüIOUEÂAÖÝÜ";
$tr = "âöçþýðüÂÖÇÞÐÜ";

undef $/;

@words = (
	  "baþlayabilirsiniz",
	  "Ýngilizce",
	  "baþladýðýmýz",
	  "alýndýktan",
	  "yapýlmaktadýr",
	  "özelliðidir",
	  "olmuþtur",
	  "çalýþmaktadýr",
	  "sonuçlarý",
	  "eriþmesi",
	  "çaðýrýlabilen",
	  "hazýrlatmýþtýk",
	  "yaratýlmýþtýr",
	  "alanýna",
	  "tanýmladýðý",
	  "baþlattýrmamýz",
	  "çalýþtýrma",
	  "alýnmamýþsa",
	  "iþleyeceðiz",
	  "seçimini",
	  "Kullanacaðýmýz",
	  "saðlayabiliriz",
	  "kütüphanesi",
	  "bittiðinde",
	  "çaðrýlacaðýndan",
	  "çaðrýsýný",
	  "farklýlýklarýndan",
	  "geçirecektir",
	  "yapýlmamasý",
	  "seçeneði",
	  "çakýþmalarý",
	  "satýrlarda",
	  "vazgeçilmez",
	  "inþaat",
	  "kaçýnýp",
	  "kullanacaðýmýz",
	  "söylemiþtik",
	  "uðraþmadan",
	  "eriþtiðinde",
	  "bakýlarak",
	  "bölümünü",
	  "baðlanýlmamasýdýr",
	  "þartlarýnda",
	  "müteþekkirim",
	  "Derneði",
	  "gördük",
	  "bölümlerde",
	  "Ýnsanlarýn",
	  "hazýrlanmýþ",
	  "anlaþýlan",
	  "birleþtirim",
	  "POJO",
	  "artýk",
	  "yapýlmaz",
	  "tanýmlamaya",
	  "alýþkanlýðý",
	  "çünkü",
	  "baþlamadan",
	  "lerinizin",
	  "sektörümüz",
	  "týklanýnca",
	  "çözüldü",
	  "yaptýrýlacaksa",
	  "tanýmlýyoruz",
	  "baþlatýldýðý",
	  "Örneðimizde",
	  "yapýlarýnýzý",
	  "iþlemlerinin",
	  "deðiþiklikler",
	  "sýkacak",
	  "baðlantýsý",
	  "menüden",
	  "karþýlaþtýrma",
	  "yazýlýmlar",
	  "çabukluðu",
	  "Saðlayýcýsý",
	  "katýlýmcýlardan",
	  "ettiðimiz",
	  "yaygýnlaþmaya",
	  "programcýlýðýn",
	  "çaðýran",
	  "öðrenmekte",
	  "teþekkürler",
	  "öðrendiðini",
	  "geçeceklerdir",
	  "takýmýnýn",
	  "paylaþýlmalý",
	  "baþlayabilecekleri",
	  "çýkarmalýdýr",
	  "baþlayacaktýr",
	  "düþüklüðü",
	  "düþünülmüþ",
	  "kýsýmdaki",
	  "kaçýnýlmazdýr",
	  "þirketlerinden",
	  "programcýnýn",
	  "seçeneklerini",
	  "aþýlamalýsýnýz",
	  "aþamasýndaydý",
	  "yýkýlmýþ",
	  "bütçelendirme",
	  "çalýþan",
	  "çalýþtýðýmýz",
	  "bakýmýndan",
	  "yarýþmasýnda",
	  "yapýlacaktýr",
	  "çaðýrýlabilecektir",
	  "görmediðimizi",
	  "gerçekleþtirmeye",
	  "baþlamýþtýr",
	  "düþünüþ",
	  "çalýþmasý",
	  "yaklaþtýrmanýn",
	  "gerçekleþtirdiðimiz",
	  "amaçlarý",
	  "iliþkisel",
	  "gelmiþlerdir",
	  "þemalarýn",
	  "seçeneklerinden",
	  "eriþiliyor",
	  "yazýlýyormuþ",
	  "deðiþtirmek",
	  "arttýrabilmeliyiz",
	  "yaptýðýmýz",
	  "iþlemini",
	  "yazýlmasýný",
	  "iþletebilmek",
	  "mantýðýný",
	  "edeceðimiz",
	  "çetrefillikte",
	  "içindeki",
	  "gerçekleþtirebiliriz",
	  "içindeki",
	  "týklamamýz",
	  "eriþebilmesi",
	  "yazýlmýþtýr",
	  "baðlanmaya",
	  "gereðinden",
	  "týklayarak",
	  "kaldýrýlmasýdýr",
	  "deðiþebilecek",
	  "baþlatýcý",
	  "týklarsanýz",
	  "hakkýnda",
	  "çýkabiliyor",
	  "güvenliðinin",
	  "týklarsanýz",
	  "eriþeceðiniz",
	  "hazýrlanmýþtýr",
	  "baþlanacaktýr",
	  "seçeneðini",
	  "geliþtiricisine",
	  "baðlantýsýndan",
	  "bakýlmaktadýr",
	  "deðiþkenine",
	  "daðýtýmýný",
	  "deðerine",
	  "Dýþarýdan",
	  "býrakýlmasýdýr",
	  "baðlanacak",
	  "arasýnda",
	  "çalýþabileceði",
	  "týkandýðý",
	  "gerçekleþtirebilmek",
	  "iþlemesini",
	  "tanýmlayabiliyoruz",
	  "deðiþkene",
	  "tanýmladýðýnýz",
	  "açmaktýr",
	  "düðmesine",
	  "eþitleyebildiði",
	  "öðrendiðimiz",
	  "deðiþmeyen",
	  "tabanýndan",
	  "gömülü",
	  "baðlantýlar",
	  "açýlmasýný",
	  "Baðlandýktan",
	  "deðiþkenlerinden",
	  "tekniðini",
	  "bölümde",
	  "tanýmlanmasý",
	  "baðlamalýyýz",
	  "edildiðine",
	  "kaçýnýrlar",
	  "yaptýrabiliriz",
	  "iþletilecektir",
	  "deðiþtirmemiz",
	  "baþlayalým",
	  "deðiþtirmemiz",
	  "baþlayabiliriz",
	  "tanýmladýðýmýz",
	  "yaklaþýmýný",
	  "kaldýrmak",
	  "döndürüldüðünde",
	  "tanýmlandý",
	  "atýlacaktýr",
	  "çevirmeye",
	  "açýlmamýþsa",
	  "iliþkilendirilmiþ",
	  "uðratýlmaktadýr",
	  "üstünde",
	  "gördüðümüz",
	  "eþlenmektedir",
	  "iþletebilirsiniz",
	  "oluþturulmuþtur",
	  "algýlanmamalýdýr",
	  "olduðumuzu",
	  "tanýmlayalým",
	  "iþleyici",
	  "bölümünde",
	  "olduðunu",
	  "gözüküyor",
	  "yapýlabilmesidir",
	  "yükümlüdür",
	  "ulaþmasýný",
	  "doðuruyor",
	  "önbelleði",
	  "Ýnsanlardan",
	  "gelmiþtir",
	  "baþlatabilen",
	  "deðerinin",
	  "deðiþikliði",
	  "sýnýrlamalardýr",
	  "tanýmlarýna",
	  "eriþebilmenizi",
	  "iþletilmesini",
	  "yapýlabilecek",
	  "konuþturacaðýz",
	  "güncellemek",
	  "alýþkýndýrlar",
	  "kýsmýný",
	  "iþlemlerdir",
	  "Þimdilik",
	  "sýfýrdan",
	  "yüzden",
	  "olduðudur",
	  "altýnda",
	  "kýsýtlamayý",
	  "yaklaþýmdýr",
	  "alýþkanlýklarý",
	  "iþletirken",
	  "kiþinin",
	  "býrakmaz",
	  "verdiðiniz",
	  "iyileþtirmeleri",
	  "iyileþtirmelerden",
	  "tanýmlanmalýdýr",
	  "baþlayarak",
	  "þartlarda",
	  "gerçekleþtirilmiþtir",
	  "açýsýndan",
	  "mümkündür",
	  "kullanýlmýþtýr",
	  "seçmek",
	  "Ýnsanlardan",
	  "danýþabilirsiniz",
	  "gözükmektedir",
	  "çevirmesine",
	  "yaklaþýmda",
	  "yazýlacak",
	  "saðlanmasýdýr",
	  "görsel",
	  "taþýmaktadýr");


foreach $f(<chap*.tex>) {
    open GIRDI, $f;    
    $_ = <GIRDI>;
    s/\\\$/\$/sg;
    
    foreach $word(@words) {
	$nn = tr_split($word);
	s/$word/$nn/sg;	
    }
    
    close GIRDI;
    open CIKTI, ">changed/$f";
    print CIKTI;
    close CIKTI;
}

#
#
sub tr_split {
    my $word = $_[0];
    my $new = "";
#    print "<< $word >>";
    $DASH = "\\-";
    $len = length($word);
    while ($len > 3) {
#	print "--";
	if (substr($word, 0, 7) =~ /[$sesli][$sessiz][$sessiz][$sesli][$sessiz][$sessiz]/) { # ilginc
#	    print "3.1\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 7) =~ /[$sessiz][$sesli][$sessiz][$sesli][$sessiz][$sesli][$sessiz]/) {
#	    print "3\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 6) =~ /[$sessiz][$sesli][$sessiz][$sessiz][$sesli][$sessiz]/) {
#	    print "5\n";
	    $new = $new . substr($word, 0, 3);
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));
	}
	if (substr($word, 0, 5) =~ /[$sesli][$sessiz][$sessiz][$sessiz][$sesli]/) { # ustte
#	    print "-1\n";
	    $new = $new . substr($word, 0, 3) ;
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));			
	}	
	if (substr($word, 0, 5) =~ /[$sesli][$sessiz][$sessiz][$sesli][$sessiz]/) { #islem
#	    print "2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 5) =~ /[$sessiz][$sesli][$sessiz][$sesli][$sessiz]/) {
#	    print "4\n";
	    $new = $new . substr($word, 0, 2);
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 5) =~  /[$sessiz][$sesli][$sessiz][$sessiz][$sesli]/) { #mikro
#	    print "7\n";
	    $new = $new . substr($word, 0, 3);
	    $new = $new . $DASH;
	    $word = substr($word, 3, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sessiz][$sesli][$sessiz]/) { 
#	    print "0\n";
	    $new = $new . substr($word, 0, 4) ;
	    $new = $new . $DASH;
	    $word = substr($word, 4, length($word));			
	}	
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sessiz][$sesli]/) {
#	    print "6\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sesli][$sessiz][$sessiz][$sesli]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sessiz][$sessiz]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 4) ;
	    $new = $new . $DASH;
	    $word = substr($word, 4, length($word));
	}
	if (substr($word, 0, 4) =~ /[$sessiz][$sesli][$sesli][$sessiz]/) {
#	    print "6.2\n";
	    $new = $new . substr($word, 0, 2) ;
	    $new = $new . $DASH;
	    $word = substr($word, 2, length($word));
	}
	if (substr($word, 0, 3) =~ /[$sesli][$sessiz][$sesli]/) {
#	    print "1\n";
	    $new = $new . substr($word, 0, 1) ;
	    $new = $new . $DASH;
	    $word = substr($word, 1, length($word));			
	}
	
	$len = length($word);

    }

    $new = $new . $word;
#    print "|| $new ||";
    return $new;
}
