library(RTOMO)

LoadRealData <- function(zOffset)
{
	MT_ST_HELENS_STNS = "../../dataset/MtStHelens/STNS"
	MT_ST_HELENS_EQKS = "../../dataset/MtStHelens/EQKS"

	#### list to return
	data = list()

	stationFile  <- file(MT_ST_HELENS_STNS, open = "r")

	stations = list()
	fileLines = 0
	stationName = NULL
	stationLat = NULL
	stationLon = NULL
	stationAlt = NULL
	while (length(oneLine <- readLines(stationFile, n = 1, warn = FALSE)) > 0)
	{
		fileLines = fileLines + 1
		if(fileLines > 2)
		{
			stationInfo = strsplit(oneLine, "[ ]")
			stationInfo[[1]] = stationInfo[[1]][!stationInfo[[1]] == ""]
			stationName = c(stationName, substr(stationInfo[[1]][1], 1, 3))
			stationLat = c(stationLat, as.double(substr(stationInfo[[1]][1], 5, 6)) + as.double(substr(stationInfo[[1]][1], 8, 12))/60.0)
			stationLon = c(stationLon, -(as.double(substr(stationInfo[[1]][2], 1, 3)) + as.double(substr(stationInfo[[1]][2], 5, 9))/60.0))
			stationAlt = c(stationAlt, as.double(stationInfo[[1]][3])/1000.0)
		}
	}

	# print(stationLat)
	# print(stationLon)
	# print(stationAlt)

	stations$name = stationName
	stations$lat = stationLat
	stations$lon = stationLon
	stations$alt = zOffset - stationAlt

	print(paste("*********************************", range(stationAlt)[1], range(stationAlt)[2]))
	print(paste("*********************************", range(stations$alt)[1], range(stations$alt)[2]))

	latRange = range(stationLat)
	lonRange = range(stationLon)

	print(latRange)
	print(lonRange)

	proj = setPROJ(2, LAT0=latRange[1], LON0=lonRange[1])
	stationsXY = GLOB.XY(stations$lat, stations$lon, proj)

	stations$x = stationsXY$x
	stations$y = stationsXY$y

	print(range(stations$x))
	print(range(stations$y))

        slon = -122.375
	slat = 46.0625
	# (-122.41, -121.965)
	# (46.0625, 46.375)

	tempXY = GLOB.XY(slat, slon, proj)
	print(tempXY)

        slon = -122.18916893005371
	slat = 46.198959

# 	slon = -122
# 	slat = 46
	# (-122.41, -121.965)
	# (46.0625, 46.375)

	tempXY = GLOB.XY(slat, slon, proj)
	print("********")
	print(tempXY)

	close(stationFile)

# 	print(stations)

	eventFile  <- file(MT_ST_HELENS_EQKS, open = "r")

	fileLines = 0
	stationLat = NULL
	stationLon = NULL
	stationAlt = NULL

	firstLine = TRUE
	eventCounter = 1
	events = array(list(), 1140)    #### total events number 1140
	eventsCoors = NULL
	rays = 0

	while (length(oneLine <- readLines(stationFile, n = 1, warn = FALSE)) > 0)
	{
		fileLines = fileLines + 1
		if(firstLine)
		{
			eventInfo = strsplit(oneLine, "[ ]")
			eventInfo[[1]] = eventInfo[[1]][!eventInfo[[1]] == ""]
			firstLine = FALSE
			if(length(eventInfo[[1]]) == 7)
			{
				events[[eventCounter]]$origin = as.double(eventInfo[[1]][3])
				events[[eventCounter]]$lat = as.double(substr(eventInfo[[1]][4], 1, 2)) + as.double(substr(eventInfo[[1]][4], 4, 8))/60.0
				events[[eventCounter]]$lon = -(as.double(substr(eventInfo[[1]][5], 1, 3)) + as.double(substr(eventInfo[[1]][5], 5, 9))/60.0)
				events[[eventCounter]]$depth = as.double(eventInfo[[1]][6]) + zOffset
				eventXY = GLOB.XY(events[[eventCounter]]$lat, events[[eventCounter]]$lon, proj)
				events[[eventCounter]]$x = eventXY$x
				events[[eventCounter]]$y = eventXY$y

				eventsCoors$lat = c(eventsCoors$lat, events[[eventCounter]]$lat)
				eventsCoors$lon = c(eventsCoors$lon, events[[eventCounter]]$lon)
				eventsCoors$depth = c(eventsCoors$depth, events[[eventCounter]]$depth)
				eventsCoors$x = c(eventsCoors$x, events[[eventCounter]]$x)
				eventsCoors$y = c(eventsCoors$y, events[[eventCounter]]$y)
			}
			if(length(eventInfo[[1]]) == 8)
			{
				events[[eventCounter]]$origin = as.double(eventInfo[[1]][4])
				events[[eventCounter]]$lat = as.double(substr(eventInfo[[1]][5], 1, 2)) + as.double(substr(eventInfo[[1]][5], 4, 8))/60.0
				events[[eventCounter]]$lon = -(as.double(substr(eventInfo[[1]][6], 1, 3)) + as.double(substr(eventInfo[[1]][6], 5, 9))/60.0)
				events[[eventCounter]]$depth = as.double(eventInfo[[1]][7]) + zOffset
				eventXY = GLOB.XY(events[[eventCounter]]$lat, events[[eventCounter]]$lon, proj)
				events[[eventCounter]]$x = eventXY$x
				events[[eventCounter]]$y = eventXY$y

				eventsCoors$lat = c(eventsCoors$lat, events[[eventCounter]]$lat)
				eventsCoors$lon = c(eventsCoors$lon, events[[eventCounter]]$lon)
				eventsCoors$depth = c(eventsCoors$depth, events[[eventCounter]]$depth)
				eventsCoors$x = c(eventsCoors$x, events[[eventCounter]]$x)
				eventsCoors$y = c(eventsCoors$y, events[[eventCounter]]$y)
			}
		}
		else
		{
			if(oneLine == "0   ")
			{
				firstLine = TRUE
				eventCounter = eventCounter + 1
			}
			else
			{
				chars = nchar(oneLine)
				stnNum = chars/14
				for(i in 1:stnNum)
				{
					start = (i-1)*14 + 1
					end = i*14
					item = substr(oneLine, start, end)
					eventInfo = strsplit(item, "[ ]")
					eventInfo[[1]] = eventInfo[[1]][!eventInfo[[1]] == ""]
					events[[eventCounter]]$station = c(events[[eventCounter]]$station, substr(eventInfo[[1]][1], 1, 3))
					events[[eventCounter]]$traveltime = c(events[[eventCounter]]$traveltime, as.double(eventInfo[[1]][2]))
					rays = rays + 1
				}
			}
		}
	}

	print("^^^^^^^^")
	print(range(eventsCoors$x))
	print(range(eventsCoors$y))
	print(range(eventsCoors$depth))
	# print(events)

	print("")

#	plot(stations$x, stations$y, xlim = c(55, 95), ylim = c(75, 120), xlab="X", ylab="Y", pch=0, cex=1.0)
#	points(eventsCoors$x, eventsCoors$y, pch=20, col = "black", cex=0.4)
#	points(80.05613, 98.56158, pch=17, col = "red", cex=1.0)
#	dev.copy2pdf(file="topology.pdf")

#	dev.new()
#	plot(eventsCoors$x, eventsCoors$depth - 3.1, xlim = c(55, 95), ylim = rev(c(0, 20)), xlab="X", ylab="Depth", pch=20, col = "black", cex=0.4, asp=0.7)
# 	points(eventsCoors$x, eventsCoors$y, pch=20, col = "black", cex=0.4)
#	dev.copy2pdf(file="depth.pdf")
	
	data$proj = proj
	data$stations = stations
	data$events = events
	data$eventsCoors = eventsCoors
	data$rays = rays

	close(eventFile)

	return(data)
}
