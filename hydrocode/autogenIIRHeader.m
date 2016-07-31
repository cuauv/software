
clear;
fid = fopen('./track_iir.hpp', 'w');

Fs = 400000;  % Sampling Frequency

N      = 10;     % Order
sections = N/2;
Apass  = 1;      % Passband Ripple (dB)
Astop  = 80;     % Stopband Attenuation (dB)


fstart = 25000;
fstop =  40000;
fstep = 500;
fprintf(fid, '#include <memory>\n');
fprintf(fid, '#include <array>\n');
fprintf(fid, '#include <unordered_map>\n\n');
fprintf(fid, 'const int NUMBER_OF_SECTIONS=%i;\n',sections);
%fprintf(fid, 'struct sosCoeffs{\n\tfloat b[NUMBER_OF_SECTIONS][3];\n\tfloat a[NUMBER_OF_SECTIONS][3];\n};\n');
fprintf(fid, 'struct sosCoeffs{\n');
fprintf(fid,'\tstd::array<float,NUMBER_OF_SECTIONS*3> b;\n');
fprintf(fid,'\tstd::array<float,NUMBER_OF_SECTIONS*3> a;\n};\n');

fprintf(fid, 'class FilterFactory\n{\n');
fprintf(fid, '\n\tpublic:\n');
fprintf(fid,' \n\tstd::unordered_map<int,std::shared_ptr<sosCoeffs> > sosMap;\n');
fprintf(fid, '\tFilterFactory()\n\t{');


%fcenter=33000
for fcenter = fstart:fstep:fstop;


Fpass1 = fcenter-fstep;
Fpass2 = fcenter+fstep;

% Construct an FDESIGN object and call its ELLIP method.
h  = fdesign.bandpass('N,Fp1,Fp2,Ast1,Ap,Ast2', N, Fpass1, Fpass2, ...
                      Astop, Apass, Astop, Fs);
Hd = design(h, 'ellip');

%Taken from MATLAB Design Functions ECE 5655/4655 Real-Time DSP 8–33

assert(strcmp(Hd.FilterStructure,'Direct-Form II, Second-Order Sections'))
dimSOS = size(Hd.sosMatrix);
Ns = dimSOS(1); % Number of biquad sections
num = zeros(Ns,3);
den = zeros(Ns,3);
for i=1:Ns,
    num(i,:) = Hd.sosMatrix(i,1:3);
    num(i,:) = num(i,:)*Hd.ScaleValues(i);
    den(i,:) = Hd.sosMatrix(i,4:6);
end 
if length(Hd.ScaleValues) == Ns+1
    assert(Hd.ScaleValues(Ns+1)==1.0,'Global scale is not 1');
end

kk = 1;
struct_name = sprintf('IIR%i',fcenter);
fprintf(fid,'\n\n', struct_name);

%fprintf(fid,'\tstd::shared_ptr<sosCoeffs> %s;\n', struct_name);
fprintf(fid,'\tauto %s = std::make_shared<sosCoeffs>();\n', struct_name);

fprintf(fid,'\t%s->b =    {',struct_name);
for i=1:Ns,
fprintf(fid,'%15.12f, %15.12f, %15.12f',num(i,1),num(i,2),num(i,3));
if i~=Ns
fprintf(fid,',');
end
end
fprintf(fid,'};\n');

fprintf(fid,'\t%s->a =    {',struct_name);
for i=1:Ns,
fprintf(fid,'%15.12f, %15.12f, %15.12f',den(i,1),den(i,2),den(i,3));
if i~=Ns
fprintf(fid,',');
end
end
    
fprintf(fid,'};\n');
fprintf(fid,'\tsosMap.insert(std::make_pair(%i,%s));',fcenter,struct_name);

%fprintf(fid,'\tsosMap.insert(std::make_pair<int,sosCoeffs >(%i,%s));',fcenter,struct_name);

%fprintf(fid,'\tsosMap.insert(std::make_pair<int,shared_ptr<sosCoeffs> >(%i,%s));',fcenter,struct_name);
end
fprintf(fid, '\t}\n');
fprintf(fid, '};');
fclose(fid);


