#Manual watershed delination
# read in HUC
#join points to HUC

#search for HUC specific flow accumulation (fa) and flow direction raster "fd" in C:\Users\jenni\Box\NAMC (Trip Armstrong)\GIS\DEMs

#if there snap point to fa raster after creating a line file of it

# then used snapped point and flow direction raster in watershed tool

HUCnum = HUC1.HUC4 # store the 4 digit HUC number as HUCnum
HUCstr = str(HUCnum) # turn the HUC number into a string
#HUCstr1 = HUCstr[0:4] # save the first 4 numbers of the HUC
#HUCstr2 = HUCstr[4:8] # save the 2nd set of 4 number of the HUC
DEM_FA = r'%sr%s/r%s_fa' % (rasterpath, HUCstr, HUCstr) # Store the needed Flow Accumulation raster
DEM_FD = r'%sr%s/r%s_fd' % (rasterpath, HUCstr, HUCstr) # Store the needed Flow Direction raster
outSnapPour = arcpy.sa.SnapPourPoint(r'%sTemp/sitetemp.shp' % (path), DEM_FA, 45, 'SiteID2') # Perform Snap Pour Point
outSnapPour.save(r'%sShedOutput/PP%s.tif' % (path, SI2str)) # Save the Snap Pour Point raster
outWatershed = arcpy.sa.Watershed(DEM_FD, outSnapPour, 'Value')    # Perform Watershed
outWatershed.save(r'%sShedOutput/WS%s.tif' % (path, SI2str)) # Save the Watershed raster
arcpy.RasterToPolygon_conversion(outWatershed, r'%sShedOutput/WS%s.shp' % (path, SI2str), 'SIMPLIFY', 'VALUE') # convert the raster watersehds to shapefiles and simplfy polygons
arcpy.AddField_management(r'%sShedOutput/WS%s.shp' % (path, SI2str), 'ShedAreakm', 'FLOAT') # Add the Area field
arcpy.CalculateField_management(r'%sShedOutput/WS%s.dbf' % (path, SI2str), 'ShedAreakm', '!SHAPE.AREA@SQUAREKILOMETERS!', 'PYTHON') # Calculate area in square kilometers
arcpy.AddField_management(r'%sShedOutput/WS%s.shp' % (path, SI2str), 'SiteID2', 'INTEGER') # Add the SiteID field
arcpy.CalculateField_management(r'%sShedOutput/WS%s.dbf' % (path, SI2str), 'SiteID2', '[GRIDCODE]') # Put in the SiteID into the SiteID field (it equals GRIDCODE)
arcpy.AddField_management(r'%sShedOutput/WS%s.shp' % (path, SI2str), 'AreaHect', 'FLOAT') # Add the Area Hectare field
arcpy.CalculateField_management(r'%sShedOutput/WS%s.dbf' % (path, SI2str), 'AreaHect', '!SHAPE.AREA@HECTARES!', 'PYTHON') # Calculate area in hectares
