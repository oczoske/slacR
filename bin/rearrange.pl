#!/usr/bin/perl -w

chomp($header = <>);
@keywords = split (' ', $header);

print "L M SN level noise v dv.low dv.high sigma dsig.low dsig.high gof rms\n";

while (<>){
    chomp;
    @values{@keywords} = split;
    $outline = join ("\t", $values{"L"}, $values{"M"}, $values{"SN"}, 
		     $values{"level"}, $values{"noise"}, $values{"v"}, 
		     $values{"dv.low"}, $values{"dv.high"}, $values{"sigma"}, 
		     $values{"dsig.low"}, $values{"dsig.high"}, $values{"gof"},
		     $values{"rms"});
    print "$outline\n";
}

    


