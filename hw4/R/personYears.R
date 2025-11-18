personYears =
  function(time1, time2 = NULL,
           event = NULL, group = NULL,
           unit = "year", timeCut = NULL) {
    if (is.null(time2)) {
      personYears = time1
    } else {
      if(any(time1 > time2)) {stop("Computed negative elapsed time")}
      personYears = time2 - time1
    }

    if(!(length(unit) == 1)) {stop("length(unit) != 1L")}

    if (!(unit %in% c("day", "week", "year", "month"))) {
      stop("Incorrect scale argument")
    } else
      if (unit == "day") {
        personYears = personYears / 365.25
        warning("(unit == \"day\") scales personYears by 365.25")
      } else
        if (unit == "week") {
          personYears = personYears / 52.1786
          warning("(unit == \"week\") scales personYears by 52.1786")
        } else
          if (unit == "month") {
            personYears = personYears / 12
          }

    if(!is.null(timeCut)) {
        timeInterval = cut(time, breaks = timeCut)
    }

    if(!is.null(event)) {
      if(length(event) != 2) {stop("length(event) != 2L")}
      event = event
    } else {event = rep(0, length(time1))}

    if(!is.null(group)) {
      if(is.vector(group)) {
        if(length(group) == length(time1)) {
          group = group
        } else {stop("length(group) != length(time1)")}
      } else {stop("group argument must be a vector")}
    }

    result = aggregate(
      data.frame(
        personYears = personYears,
        events = event,
        n = 1),
    by = list(group, timeInterval),
    FUN = sum
    )

    return(result)

    }


