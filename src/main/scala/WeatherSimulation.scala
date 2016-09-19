/**
  * Created by Shiva on 19-09-2016.
  */
import java.text.DecimalFormat;

object WeatherSimulation {
  //Topological location using elevation as well
  class TopoLoc(name: String,latitude: Double,longitude: Double,elevation: Double) {
    def getName(): String = return name;
    def getLatitude():Double = return latitude;
    def getLongitude():Double = return longitude;
    def getElevation():Double = return elevation;

    override def toString : String = name+"|"+latitude+","+longitude+","+elevation
  }


  //Class for dates
  class DateTimeISO8601(year: Int=2016, month: Int=1,date: Int=1,hour: Int=0,minute: Int=0,second: Int=0) {
    private var feb=28;
    if(year%4 == 0)
      feb+=1;
    private var daysInMonths = Array(31,feb,31,30,31,30,31,31,30,31,30,31);

    if(month > 12 || date > daysInMonths(month-1)) {println("Wrong date");System.exit(1)}

    //overloaded operator to get new date after adding the proper values
    def +(hoursParam:Int, minutesParam:Int, secondsParam: Int): DateTimeISO8601 = {
      var hours = hoursParam;
      var minutes =  minutesParam;
      var seconds = secondsParam;
      var year = this.year;
      var month = this.month;
      var date = this.date;
      var hour = this.hour;
      var minute = this.minute;
      var second = this.second;

      var actual_seconds:Int = seconds%60;
      minutes += (seconds/60);
      var actual_minutes: Int = minutes%60;
      hours += (minutes/60);
      var actual_hours: Int = hours%24;
      var days: Int = (hours/24);
      while(days > daysInMonths(month-1))
      {
        days -= ((daysInMonths(month-1) - date) + 1);
        date = 1;
        month+=1;
        if(month > 12){
          month = 1;
          year+=1;
        }
      }
      date += days;
      second += actual_seconds;minute += (second/60);second = second%60;
      minute += actual_minutes;hour += (minute/60);minute = minute%60;
      hour += actual_hours;date += (hour/24);hour = hour%24;
      if(date > daysInMonths(month-1)){
        date -= daysInMonths(month-1);
        month+=1;
        if(month > 12) {
          month = 1;
          year+=1;
        }
      }
      return new DateTimeISO8601(year,month,date,hour,minute,second);
    }

    def getHour: Int = hour;

    //To make single digit numbers as double digit..
    def getString(value: Int) : String = {
      if(value < 10)
        return "0"+value;
      else
        return value+"";
    }

    override def toString: String = year+"-"+getString(month)+"-"+getString(date)+"T"+getString(hour)+":"+getString(minute)+":"+getString(second)+"Z";
  }


  // Get wind contribution in affecting temperature and pressure.. this is random value
  // to give us some variance
  // wind + humidity usually decreases the temperature.. so used it for some randomness
  // in data
  def getWindEffect : Double = (scala.util.Random).nextInt(20)/10;

  // Get sun intensity based upon the time of the day
  // It is highest at 12 noon.. 100% intensity
  def getSunIntensity(hour: Int): Double = {
    return 100.0 - (math.abs(hour - 12) * 100.0/12.0);
  }

  // Assuming, farther the place from the ocean, less humid it would be..
  // so from a central point (-22.5,135) calculating distance of other locations
  // closer to the center would mean less humidity.
  def oceanDistanceAffect(location: TopoLoc) : Double = {
    var lat_cent: Double = -22.5;
    var long_cent: Double = 135;
    var dist: Double = math.sqrt(math.pow(lat_cent - location.getLatitude(),2)+math.pow(long_cent- location.getLongitude(),2));
    var variance : Double = (scala.util.Random).nextInt(50)/10;
    //just substracted some random variance
    return (dist*100/22 - variance);
  }

  // calculating humidity by giving weightage of 5:1 to closeness to ocean and
  // elevation above the sea level
  def getHumidity(location: TopoLoc) : Double = {
    return ((5)*oceanDistanceAffect(location) + 1*(location.getElevation()/1500)*100)/6;
  }

  // calculating pressure by assuming a mean of 1000 that gets displaced by temperature,
  // wind + humidity and elevation from the ground.. all of these affect it inversely..
  // so we substract fractions of them from 1000
  // elevation is normalized by 1500 m so that we dont get undue contribution from low elevations
  // even 1500 is low yet it works ok in our case..
  //it is assumed that 0.7 fraction of temperature, 0.7 fraction of combined effect of wind+ humidity
  // and 0.05 of normalized elevation decrease the pressure somewhat from 1000 units
  def getPressure(temp: Double, humid: Double, elevation: Double): Double = {
    return 1000 - (temp*(70/100) + getWindEffect*humid*(70/100) + (elevation/1500)*50/100)
  }

  // collect all other stats other than location and time here
  def getStat(location: TopoLoc,hour: Int): String = {
    var sun : Double = getSunIntensity(hour);
    var humidity : Double = getHumidity(location);
    // assuming that 4/50th of suns intensity helps in increasing temperature and 1/50 of the total
    // affect of humidity is assumed so as to give us better looking temperature values..
    // still these values would be  very high if we don't consider the effects of phenomenons
    // that may decrease the temperature those are elevation from the sea level (normalized) and himidity+wind
    // causing the temperature to fall
    var temperature : Double = ((4.0*sun) + (1.0*humidity)/5.0)*(10.0/100.0) - (humidity*getWindEffect*5.0/100.0 + (location.getElevation/1500.0)*(70.0/100.0));
    var pressure : Double = getPressure(temperature,humidity,location.getElevation);

    var cloudStatus : String = "Sunny";
    if(temperature < 0)
      cloudStatus = "Snow";
    else if(temperature < 15)
    {
      if(humidity > 60)
        cloudStatus = "Rainy";
    }
    else if(temperature < 30)
    {
      if(humidity > 80)
        cloudStatus = "Rainy";
    }
    else
      cloudStatus = "Sunny";
    val formatter = new DecimalFormat("####.#");
    return cloudStatus+"|"+ (if(temperature>0)"+" else "-")+formatter.format(temperature)+"|"+formatter.format(pressure)+"|"+(humidity.toInt);
  }

  def initLocation(): List[TopoLoc] = {
    return List(
      new TopoLoc("Sydney",-33.86,151.207,34),
      new TopoLoc("Melbourne",-31.81,144.96,33),
      new TopoLoc("Brisbane",-27.47,153.02,150),
      new TopoLoc("Perth",-31.95,115.86,300),
      new TopoLoc("Adelaide",-34.93,138.56,40),
      new TopoLoc("Gold Coast",-28,153.43,6),
      new TopoLoc("Canberra",-35.28,149.13,88),
      new TopoLoc("Newcastle",-32.93,151.78,180),
      new TopoLoc("Wollongong",-34.43,150.89,350),
      new TopoLoc("Logan City",-27.64,153.11,64)
    );
  }

  def main(args: Array[String]) {
    var locations = initLocation();
    var startDate = new DateTimeISO8601(2015,11,10,(scala.util.Random).nextInt(24),(scala.util.Random).nextInt(61),(scala.util.Random).nextInt(61));
    var date  = startDate;
    var a = 0;
    for(a <- 1 to 10) {
      for(l <- locations)
      {
        println(l + "|"+date+"|" +getStat(l,date.getHour))
      }
      date = date+((scala.util.Random).nextInt(48),(scala.util.Random).nextInt(61),(scala.util.Random).nextInt(61));
    }
  }
}
