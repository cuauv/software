#include <memory>
#include <array>
#include <unordered_map>

const int NUMBER_OF_SECTIONS=5;
struct sosCoeffs{
	std::array<float,NUMBER_OF_SECTIONS*3> b;
	std::array<float,NUMBER_OF_SECTIONS*3> a;
};
class FilterFactory
{

	public:
 
	std::unordered_map<int,std::shared_ptr<sosCoeffs> > sosMap;
	FilterFactory()
	{

	auto IIR25000 = std::make_shared<sosCoeffs>();
	IIR25000->b =    { 0.210360920090, -0.385248230922,  0.210360920090, 0.210360920090, -0.391842943416,  0.210360920090, 0.163760429329, -0.298223372607,  0.163760429329, 0.163760429329, -0.306343495081,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR25000->a =    { 1.000000000000, -1.840532917515,  0.998687545961, 1.000000000000, -1.852494770417,  0.998735973235, 1.000000000000, -1.840552862202,  0.996299302381, 1.000000000000, -1.848230975230,  0.996386956491, 1.000000000000, -1.843421586918,  0.995243607621};
	sosMap.insert(std::make_pair(25000,IIR25000));

	auto IIR25500 = std::make_shared<sosCoeffs>();
	IIR25500->b =    { 0.210360920090, -0.383909964706,  0.210360920090, 0.210360920090, -0.390629215312,  0.210360920090, 0.163760429329, -0.297154313337,  0.163760429329, 0.163760429329, -0.305427135827,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR25500->a =    { 1.000000000000, -1.834356664861,  0.998688073846, 1.000000000000, -1.846542992288,  0.998735445325, 1.000000000000, -1.834424904561,  0.996300258051, 1.000000000000, -1.842244839039,  0.996386000737, 1.000000000000, -1.837367726606,  0.995243607621};
	sosMap.insert(std::make_pair(25500,IIR25500));

	auto IIR26000 = std::make_shared<sosCoeffs>();
	IIR26000->b =    { 0.210360920090, -0.382548013927,  0.210360920090, 0.210360920090, -0.389391394546,  0.210360920090, 0.163760429329, -0.296066913468,  0.163760429329, 0.163760429329, -0.304491947110,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR26000->a =    { 1.000000000000, -1.828067239578,  0.998688582504, 1.000000000000, -1.840477331558,  0.998734936642, 1.000000000000, -1.828183753488,  0.996301178902, 1.000000000000, -1.836145101973,  0.996385079808, 1.000000000000, -1.831200528798,  0.995243607621};
	sosMap.insert(std::make_pair(26000,IIR26000));

	auto IIR26500 = std::make_shared<sosCoeffs>();
	IIR26500->b =    { 0.210360920090, -0.381162462767,  0.210360920090, 0.210360920090, -0.388129557302,  0.210360920090, 0.163760429329, -0.294961240646,  0.163760429329, 0.163760429329, -0.303537986046,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR26500->a =    { 1.000000000000, -1.821665030747,  0.998689073028, 1.000000000000, -1.834298161268,  0.998734446096, 1.000000000000, -1.821829795974,  0.996302066911, 1.000000000000, -1.829932138283,  0.996384191724, 1.000000000000, -1.824920373915,  0.995243607621};
	sosMap.insert(std::make_pair(26500,IIR26500));

	auto IIR27000 = std::make_shared<sosCoeffs>();
	IIR27000->b =    { 0.210360920090, -0.379753396851,  0.210360920090, 0.210360920090, -0.386843781258,  0.210360920090, 0.163760429329, -0.293837363603,  0.163760429329, 0.163760429329, -0.302565310952,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR27000->a =    { 1.000000000000, -1.815150434320,  0.998689546427, 1.000000000000, -1.828005861542,  0.998733972676, 1.000000000000, -1.815363425820,  0.996302923908, 1.000000000000, -1.823606329356,  0.996383334658, 1.000000000000, -1.818527649347,  0.995243607621};
	sosMap.insert(std::make_pair(27000,IIR27000));

	auto IIR27500 = std::make_shared<sosCoeffs>();
	IIR27500->b =    { 0.210360920090, -0.378320903244,  0.210360920090, 0.210360920090, -0.385534145581,  0.210360920090, 0.163760429329, -0.292695352158,  0.163760429329, 0.163760429329, -0.301573981333,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR27500->a =    { 1.000000000000, -1.808523853112,  0.998690003638, 1.000000000000, -1.821600819557,  0.998733515445, 1.000000000000, -1.808785043631,  0.996303751590, 1.000000000000, -1.817168063668,  0.996382506909, 1.000000000000, -1.812022749428,  0.995243607621};
	sosMap.insert(std::make_pair(27500,IIR27500));

	auto IIR28000 = std::make_shared<sosCoeffs>();
	IIR28000->b =    { 0.210360920090, -0.376865070445,  0.210360920090, 0.210360920090, -0.384200730918,  0.210360920090, 0.163760429329, -0.291535277212,  0.163760429329, 0.163760429329, -0.300564057884,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR28000->a =    { 1.000000000000, -1.801785696774,  0.998690445531, 1.000000000000, -1.815083429511,  0.998733073533, 1.000000000000, -1.802095056797,  0.996304551534, 1.000000000000, -1.810617736758,  0.996381706903, 1.000000000000, -1.805406075411,  0.995243607621};
	sosMap.insert(std::make_pair(28000,IIR28000));

	auto IIR28500 = std::make_shared<sosCoeffs>();
	IIR28500->b =    { 0.210360920090, -0.375385988384,  0.210360920090, 0.210360920090, -0.382843619394,  0.210360920090, 0.163760429329, -0.290357210749,  0.163760429329, 0.163760429329, -0.299535602476,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR28500->a =    { 1.000000000000, -1.794936381780,  0.998690872915, 1.000000000000, -1.808454092598,  0.998732646131, 1.000000000000, -1.795293879483,  0.996305325205, 1.000000000000, -1.803955751186,  0.996380933173, 1.000000000000, -1.798678035442,  0.995243607621};
	sosMap.insert(std::make_pair(28500,IIR28500));

	auto IIR29000 = std::make_shared<sosCoeffs>();
	IIR29000->b =    { 0.210360920090, -0.373883748415,  0.210360920090, 0.210360920090, -0.381462894603,  0.210360920090, 0.163760429329, -0.289161225835,  0.163760429329, 0.163760429329, -0.298488678151,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR29000->a =    { 1.000000000000, -1.787976331402,  0.998691286543, 1.000000000000, -1.801713216970,  0.998732232485, 1.000000000000, -1.788381932608,  0.996306073966, 1.000000000000, -1.797182516503,  0.996380184355, 1.000000000000, -1.791839044540,  0.995243607621};
	sosMap.insert(std::make_pair(29000,IIR29000));

	auto IIR29500 = std::make_shared<sosCoeffs>();
	IIR29500->b =    { 0.210360920090, -0.372358443314,  0.210360920090, 0.210360920090, -0.380058641606,  0.210360920090, 0.163760429329, -0.287947396615,  0.163760429329, 0.163760429329, -0.297423349118,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR29500->a =    { 1.000000000000, -1.780905975694,  0.998691687118, 1.000000000000, -1.794861217712,  0.998731831894, 1.000000000000, -1.781359643833,  0.996306799092, 1.000000000000, -1.790298449215,  0.996379459177, 1.000000000000, -1.784889524565,  0.995243607621};
	sosMap.insert(std::make_pair(29500,IIR29500));

	auto IIR30000 = std::make_shared<sosCoeffs>();
	IIR30000->b =    { 0.210360920090, -0.370810167272,  0.210360920090, 0.210360920090, -0.378630946920,  0.210360920090, 0.163760429329, -0.286715798309,  0.163760429329, 0.163760429329, -0.296339680746,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR30000->a =    { 1.000000000000, -1.773725751463,  0.998692075294, 1.000000000000, -1.787898516813,  0.998731443702, 1.000000000000, -1.774227447539,  0.996307501767, 1.000000000000, -1.783303972751,  0.996378756450, 1.000000000000, -1.777829904197,  0.995243607621};
	sosMap.insert(std::make_pair(30000,IIR30000));

	auto IIR30500 = std::make_shared<sosCoeffs>();
	IIR30500->b =    { 0.210360920090, -0.369239015891,  0.210360920090, 0.210360920090, -0.377179898514,  0.210360920090, 0.163760429329, -0.285466507213,  0.163760429329, 0.163760429329, -0.295237739557,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR30500->a =    { 1.000000000000, -1.766436102252,  0.998692451685, 1.000000000000, -1.780825543133,  0.998731067297, 1.000000000000, -1.766985784810,  0.996308183103, 1.000000000000, -1.776199517427,  0.996378075066, 1.000000000000, -1.770660618906,  0.995243607621};
	sosMap.insert(std::make_pair(30500,IIR30500));

	auto IIR31000 = std::make_shared<sosCoeffs>();
	IIR31000->b =    { 0.210360920090, -0.367645086178,  0.210360920090, 0.210360920090, -0.375705585807,  0.210360920090, 0.163760429329, -0.284199600691,  0.163760429329, 0.163760429329, -0.294117593219,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR31000->a =    { 1.000000000000, -1.759037478313,  0.998692816864, 1.000000000000, -1.773642732373,  0.998730702104, 1.000000000000, -1.759635103407,  0.996308844137, 1.000000000000, -1.768985520417,  0.996377413986, 1.000000000000, -1.763382110927,  0.995243607621};
	sosMap.insert(std::make_pair(31000,IIR31000));

	auto IIR31500 = std::make_shared<sosCoeffs>();
	IIR31500->b =    { 0.210360920090, -0.366028476537,  0.210360920090, 0.210360920090, -0.374208099656,  0.210360920090, 0.163760429329, -0.282915157178,  0.163760429329, 0.163760429329, -0.292979310544,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR31500->a =    { 1.000000000000, -1.751530336582,  0.998693171367, 1.000000000000, -1.766350527049,  0.998730347587, 1.000000000000, -1.752175857753,  0.996309485842, 1.000000000000, -1.761662425717,  0.996376772237, 1.000000000000, -1.755994829233,  0.995243607621};
	sosMap.insert(std::make_pair(31500,IIR31500));

	auto IIR32000 = std::make_shared<sosCoeffs>();
	IIR32000->b =    { 0.210360920090, -0.364389286768,  0.210360920090, 0.210360920090, -0.372687532354,  0.210360920090, 0.163760429329, -0.281613256171,  0.163760429329, 0.163760429329, -0.291822961480,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR32000->a =    { 1.000000000000, -1.743915140657,  0.998693515698, 1.000000000000, -1.758949376457,  0.998730003244, 1.000000000000, -1.744608508903,  0.996310109128, 1.000000000000, -1.754230684117,  0.996376148910, 1.000000000000, -1.748499229507,  0.995243607621};
	sosMap.insert(std::make_pair(32000,IIR32000));

	auto IIR32500 = std::make_shared<sosCoeffs>();
	IIR32500->b =    { 0.210360920090, -0.362727618060,  0.210360920090, 0.210360920090, -0.371143977621,  0.210360920090, 0.163760429329, -0.280293978228,  0.163760429329, 0.163760429329, -0.290648617104,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR32500->a =    { 1.000000000000, -1.736192360767,  0.998693850329, 1.000000000000, -1.751439736647,  0.998729668601, 1.000000000000, -1.736933524525,  0.996310714852, 1.000000000000, -1.746690753162,  0.996375543146, 1.000000000000, -1.740895774113,  0.995243607621};
	sosMap.insert(std::make_pair(32500,IIR32500));

	auto IIR33000 = std::make_shared<sosCoeffs>();
	IIR33000->b =    { 0.210360920090, -0.361043572982,  0.210360920090, 0.210360920090, -0.369577530602,  0.210360920090, 0.163760429329, -0.278957404965,  0.163760429329, 0.163760429329, -0.289456349620,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR33000->a =    { 1.000000000000, -1.728362473751,  0.998694175703, 1.000000000000, -1.743822070387,  0.998729343215, 1.000000000000, -1.729151378874,  0.996311303817, 1.000000000000, -1.739043097124,  0.996374954143, 1.000000000000, -1.733184932066,  0.995243607621};
	sosMap.insert(std::make_pair(33000,IIR33000));

	auto IIR33500 = std::make_shared<sosCoeffs>();
	IIR33500->b =    { 0.210360920090, -0.359337255480,  0.210360920090, 0.210360920090, -0.367988287855,  0.210360920090, 0.163760429329, -0.277603619051,  0.163760429329, 0.163760429329, -0.288246232349,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR33500->a =    { 1.000000000000, -1.720425963025,  0.998694492239, 1.000000000000, -1.736096847138,  0.998729026669, 1.000000000000, -1.721262552768,  0.996311876779, 1.000000000000, -1.731288186970,  0.996374381145, 1.000000000000, -1.725367179009,  0.995243607621};
	sosMap.insert(std::make_pair(33500,IIR33500));

	auto IIR34000 = std::make_shared<sosCoeffs>();
	IIR34000->b =    { 0.210360920090, -0.357608770869,  0.210360920090, 0.210360920090, -0.366376347351,  0.210360920090, 0.163760429329, -0.276232704203,  0.163760429329, 0.163760429329, -0.287018339728,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR34000->a =    { 1.000000000000, -1.712383318559,  0.998694800328, 1.000000000000, -1.728264543021,  0.998728718569, 1.000000000000, -1.713267533559,  0.996312434449, 1.000000000000, -1.723426500326,  0.996373823440, 1.000000000000, -1.717442997177,  0.995243607621};
	sosMap.insert(std::make_pair(34000,IIR34000));

	auto IIR34500 = std::make_shared<sosCoeffs>();
	IIR34500->b =    { 0.210360920090, -0.355858225831,  0.210360920090, 0.210360920090, -0.364741808462,  0.210360920090, 0.163760429329, -0.274844745183,  0.163760429329, 0.163760429329, -0.285772747301,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR34500->a =    { 1.000000000000, -1.704235036845,  0.998695100341, 1.000000000000, -1.720325640785,  0.998728418545, 1.000000000000, -1.705166815108,  0.996312977498, 1.000000000000, -1.715458521448,  0.996373280358, 1.000000000000, -1.709412875371,  0.995243607621};
	sosMap.insert(std::make_pair(34500,IIR34500));

	auto IIR35000 = std::make_shared<sosCoeffs>();
	IIR35000->b =    { 0.210360920090, -0.354085728402,  0.210360920090, 0.210360920090, -0.363084771960,  0.210360920090, 0.163760429329, -0.273439827793,  0.163760429329, 0.163760429329, -0.284509531716,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR35000->a =    { 1.000000000000, -1.695981620872,  0.998695392628, 1.000000000000, -1.712280629774,  0.998728126249, 1.000000000000, -1.696960897757,  0.996313506557, 1.000000000000, -1.707384741186,  0.996372751267, 1.000000000000, -1.701277308927,  0.995243607621};
	sosMap.insert(std::make_pair(35000,IIR35000));

	auto IIR35500 = std::make_shared<sosCoeffs>();
	IIR35500->b =    { 0.210360920090, -0.352291387970,  0.210360920090, 0.210360920090, -0.361405340006,  0.210360920090, 0.163760429329, -0.272018038873,  0.163760429329, 0.163760429329, -0.283228770718,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR35500->a =    { 1.000000000000, -1.687623580092,  0.998695677517, 1.000000000000, -1.704130005900,  0.998727841351, 1.000000000000, -1.688650288303,  0.996314022224, 1.000000000000, -1.699205656954,  0.996372235570, 1.000000000000, -1.693036799685,  0.995243607621};
	sosMap.insert(std::make_pair(35500,IIR35500));

	auto IIR36000 = std::make_shared<sosCoeffs>();
	IIR36000->b =    { 0.210360920090, -0.350475315269,  0.210360920090, 0.210360920090, -0.359703616146,  0.210360920090, 0.163760429329, -0.270579466290,  0.163760429329, 0.163760429329, -0.281930543142,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR36000->a =    { 1.000000000000, -1.679161430393,  0.998695955319, 1.000000000000, -1.695874271607,  0.998727563539, 1.000000000000, -1.680235499964,  0.996314525062, 1.000000000000, -1.690921772693,  0.996371732703, 1.000000000000, -1.684691855958,  0.995243607621};
	sosMap.insert(std::make_pair(36000,IIR36000));

	auto IIR36500 = std::make_shared<sosCoeffs>();
	IIR36500->b =    { 0.210360920090, -0.348637622369,  0.210360920090, 0.210360920090, -0.357979705302,  0.210360920090, 0.163760429329, -0.269124198942,  0.163760429329, 0.163760429329, -0.280614928912,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR36500->a =    { 1.000000000000, -1.670595694067,  0.998696226330, 1.000000000000, -1.687513935842,  0.998727292520, 1.000000000000, -1.671717052355,  0.996315015603, 1.000000000000, -1.682533598841,  0.996371242133, 1.000000000000, -1.676242992502,  0.995243607621};
	sosMap.insert(std::make_pair(36500,IIR36500));

	auto IIR37000 = std::make_shared<sosCoeffs>();
	IIR37000->b =    { 0.210360920090, -0.346778422673,  0.210360920090, 0.210360920090, -0.356233713769,  0.210360920090, 0.163760429329, -0.267652326745,  0.163760429329, 0.163760429329, -0.279282009033,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR37000->a =    { 1.000000000000, -1.661926899779,  0.998696490826, 1.000000000000, -1.679049514018,  0.998727028016, 1.000000000000, -1.663095471456,  0.996315494352, 1.000000000000, -1.674041652300,  0.996370763358, 1.000000000000, -1.667690730482,  0.995243607621};
	sosMap.insert(std::make_pair(37000,IIR37000));

	auto IIR37500 = std::make_shared<sosCoeffs>();
	IIR37500->b =    { 0.210360920090, -0.344897830906,  0.210360920090, 0.210360920090, -0.354465749208,  0.210360920090, 0.163760429329, -0.266163940634,  0.163760429329, 0.163760429329, -0.277931865582,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR37500->a =    { 1.000000000000, -1.653155582537,  0.998696749073, 1.000000000000, -1.670481527986,  0.998726769762, 1.000000000000, -1.654371289582,  0.996315961785, 1.000000000000, -1.665446456397,  0.996370295899, 1.000000000000, -1.659035597443,  0.995243607621};
	sosMap.insert(std::make_pair(37500,IIR37500));

	auto IIR38000 = std::make_shared<sosCoeffs>();
	IIR38000->b =    { 0.210360920090, -0.342995963112,  0.210360920090, 0.210360920090, -0.352675920632,  0.210360920090, 0.163760429329, -0.264659132552,  0.163760429329, 0.163760429329, -0.276564581708,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR38000->a =    { 1.000000000000, -1.644282283659,  0.998697001319, 1.000000000000, -1.661810505998,  0.998726517508, 1.000000000000, -1.645545045349,  0.996316418355, 1.000000000000, -1.656748540856,  0.996369839305, 1.000000000000, -1.650278127273,  0.995243607621};
	sosMap.insert(std::make_pair(38000,IIR38000));

	auto IIR38500 = std::make_shared<sosCoeffs>();
	IIR38500->b =    { 0.210360920090, -0.341072936646,  0.210360920090, 0.210360920090, -0.350864338411,  0.210360920090, 0.163760429329, -0.263137995452,  0.163760429329, 0.163760429329, -0.275180241625,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR38500->a =    { 1.000000000000, -1.635307550738,  0.998697247800, 1.000000000000, -1.653036982675,  0.998726271019, 1.000000000000, -1.636617283647,  0.996316864490, 1.000000000000, -1.647948441760,  0.996369393145, 1.000000000000, -1.641418860175,  0.995243607621};
	sosMap.insert(std::make_pair(38500,IIR38500));

	auto IIR39000 = std::make_shared<sosCoeffs>();
	IIR39000->b =    { 0.210360920090, -0.339128870164,  0.210360920090, 0.210360920090, -0.349031114255,  0.210360920090, 0.163760429329, -0.261600623285,  0.163760429329, 0.163760429329, -0.273778930604,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR39000->a =    { 1.000000000000, -1.626231937615,  0.998697488743, 1.000000000000, -1.644161498975,  0.998726030069, 1.000000000000, -1.627588555604,  0.996317300599, 1.000000000000, -1.639046701520,  0.996368957014, 1.000000000000, -1.632458342629,  0.995243607621};
	sosMap.insert(std::make_pair(39000,IIR39000));

	auto IIR39500 = std::make_shared<sosCoeffs>();
	IIR39500->b =    { 0.210360920090, -0.337163883621,  0.210360920090, 0.210360920090, -0.347176361211,  0.210360920090, 0.163760429329, -0.260047110999,  0.163760429329, 0.163760429329, -0.272360734970,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR39500->a =    { 1.000000000000, -1.617056004339,  0.998697724360, 1.000000000000, -1.635184602155,  0.998725794446, 1.000000000000, -1.618459418558,  0.996317727066, 1.000000000000, -1.630043868833,  0.996368530525, 1.000000000000, -1.623397127364,  0.995243607621};
	sosMap.insert(std::make_pair(39500,IIR39500));

	auto IIR40000 = std::make_shared<sosCoeffs>();
	IIR40000->b =    { 0.210360920090, -0.335178098257,  0.210360920090, 0.210360920090, -0.345300193657,  0.210360920090, 0.163760429329, -0.258477554532,  0.163760429329, 0.163760429329, -0.270925742093,  0.163760429329, 0.007835464436,  0.000000000000, -0.007835464436};
	IIR40000->a =    { 1.000000000000, -1.607780317139,  0.998697954855, 1.000000000000, -1.626106845740,  0.998725563945, 1.000000000000, -1.609230436018,  0.996318144259, 1.000000000000, -1.620940498656,  0.996368113311, 1.000000000000, -1.614235773316,  0.995243607621};
	sosMap.insert(std::make_pair(40000,IIR40000));	}
};