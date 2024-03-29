
# == title
# Draw links between points or/and intervals
#
# == param
# -sector.index1 Index for the first sector where one link end locates
# -point1        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -sector.index2 Index for the other sector where the other link end locates
# -point2        A single value or a numeric vector of length 2. If it is a 2-elements vector, then
#                the link would be a belt/ribbon.
# -rou           The position of the the link ends (if ``rou1`` and ``rou2`` are not set). It is the percentage of the radius of the unit circle.
#                By default its value is the position of bottom margin of the most inner track.
# -rou1          The position of end 1 of the link.
# -rou2          The position of end 2 of the link.
# -h             Height of the link, measured as percent to the radius to the unit circle. By default it is automatically infered.
# -h.ratio       systematically change the link height. The value is between 0 and 1.
# -w             Since the link is a Bezier curve, it controls the shape of Bezier curve.
# -h2            Height of the bottom edge of the link if it is a ribbon.
# -w2            Shape of the bottom edge of the link if it is a ribbon.
# -inverse       Whether the link is inversed.
# -col           Color of the link. If the link is a ribbon, then it is the filled color for the ribbon.
# -lwd           Line (or border) width
# -lty           Line (or border) style
# -border        If the link is a ribbon, then it is the color for the ribbon border.
# -directional   0 for no direction, 1 for direction from ``point1`` to ``point2``, -1 for direction from ``point2`` to ``point1``.
#                2 for two directional. The direction is important when arrow heads are added.
# -arr.width     Width of the arrows, pass to `shape::Arrowhead`.
# -arr.type      Type of the arrows, pass to `shape::Arrowhead`. Default value is ``triangle``. There is an additional option
#                ``big.arrow``.
# -arr.length    Length of the arrows, measured in 'cm', pass to `shape::Arrowhead`. If ``arr.type`` is set to ``big.arrow``,
#                the value is percent to the radius of the unit circle.
# -arr.col       Color of the arrows, pass to `shape::Arrowhead`.
# -arr.lwd       Line width of arrows, pass to `shape::Arrowhead`.
# -arr.lty       Line type of arrows, pass to `shape::Arrowhead`.
# -reduce_to_mid_line Only use the middle points of ``point1`` and ``point2`` to draw the link.
#
# == details
# Links are implemented as quadratic Bezier curves (https://en.wikipedia.org/wiki/B\%C3\%A9zier_curve#Rational_B.C3.A9zier_curves ).
#
# Drawing links does not create any track. So you can think it is independent of the tracks.
#
# By default you only need to set ``sector.index1``, ``point1``, ``sector.index2`` and ``point2``. The
# links would look nice.
#
# Please refer to the vignette for detailed explanation.
#
# == seealso
# https://jokergoo.github.io/circlize_book/book/graphics.html#links
#
circos.link = function(
	sector.index1,
	point1,
	sector.index2,
	point2,
    rou = get_most_inside_radius(),
    rou1 = rou,
    rou2 = rou,
    h = NULL,
    h.ratio = 0.5,
    w = 1,
    h2 = h,
    w2 = w,
    inverse = FALSE,
    col = "black",
    lwd = par("lwd"),
    lty = par("lty"),
    border = col,
    directional = 0,
    arr.length = ifelse(arr.type == "big.arrow", 0.02, 0.4),
    arr.width = arr.length/2,
    arr.type = "triangle",
    arr.lty = lty,
    arr.lwd = lwd,
    arr.col = col,
    reduce_to_mid_line = FALSE) {

   sector.data1 = get.sector.data(sector.index1)
   sector.data2 = get.sector.data(sector.index2)

   if(is.data.frame(point1)) {
   		point1 = unlist(point1)
   }
   if(is.data.frame(point2)) {
   		point2 = unlist(point2)
   }
	
   if(circos.par$ring) {
   	if(length(point1) == 2) {
   		if(point1[1] > point1[2]) {
   			point1[1] = point1[1] - diff(get.sector.data()[c("min.value", "max.value")])
   		}
   	}

   	if(length(point2) == 2) {
   		if(point2[1] > point2[2]) {
   			point2[1] = point2[1] - diff(get.sector.data()[c("min.value", "max.value")])
   		}
   	}
   }

	point1 = sort(point1)
	point2 = sort(point2)

	rg1 = sector.data1["max.data"] - sector.data1["min.data"]
	if(min(point1) < sector.data1["min.data"] - rg1*1e-6 || max(point1) > sector.data1["max.data"] + rg1*1e-6) {
		message_wrap(paste0("Note: The first link end is drawn out of sector '", sector.index1, "'."))
	}
	rg2 = sector.data2["max.data"] - sector.data2["min.data"]
	if(min(point2) < sector.data2["min.data"] - rg2*1e-6 || max(point2) > sector.data2["max.data"] + rg2*1e-6) {
		message_wrap(paste0("Note: The second link end is drawn out of sector '", sector.index2, "'."))
	}

	if(reduce_to_mid_line) {
		point1 = mean(point1)
		point2 = mean(point2)
	}

    if(length(point1) == 1 && length(point2) == 1) {  # single line
        theta1 = circlize(point1, 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta2 = circlize(point2, 0, sector.index = sector.index2, track.index = 0)[1, "theta"]

		d = getQuadraticPoints(theta1, theta2, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
        nr = nrow(d)
        # if(directional == 0) {
        	lines(d, col = col, lwd = lwd, lty = lty, lend = "butt")
  #       } else if(directional == 1) {
		# 	lines(d[-nr, , drop = FALSE], col = col, lwd = lwd, lty = lty, lend = "butt")
		# } else if(directional == -1) {
		# 	lines(d[-1, , drop = FALSE], col = col, lwd = lwd, lty = lty, lend = "butt")
		# } else if(directional == 2) {
		# 	lines(d[-c(1, nr), , drop = FALSE], col = col, lwd = lwd, lty = lty, lend = "butt")
		# }

        if(nrow(d) > 1) {
	        if(directional %in% c(1,2)) {  # point1 to point2
	        	alpha = line_degree(d[nr-1, 1], d[nr-1, 2], d[nr, 1], d[nr, 2])
	        	oljoin = par("ljoin")
	        	par(ljoin = "mitre")
	        	Arrowhead(d[nr, 1], d[nr, 2], alpha, arr.length = arr.length, arr.width = arr.width,
	        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = col, lcol = col)
	        	par(ljoin = oljoin)
	        }
	        if(directional %in% c(-1,2)) {  # point2 to point2
	        	alpha = line_degree(d[2, 1], d[2, 2], d[1, 1], d[1,2])
	        	oljoin = par("ljoin")
	        	par(ljoin = "mitre")
	        	Arrowhead(d[1, 1], d[1, 2], alpha, arr.length = arr.length, arr.width = arr.width,
	        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = col, lcol = col)
	        	par(ljoin = oljoin)
	        }
	    }
    } else if(length(point1) == 1) {
		theta1 = circlize(point1, 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
		theta21 = circlize(point2[1], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        theta22 = circlize(point2[2], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]

        if(degreeDiff2(theta1, theta21) <= degreeDiff(theta22, theta21) &
           degreeDiff2(theta22, theta1) <= degreeDiff(theta22, theta21)) {
			d = getQuadraticPoints(theta22, theta21, max(rou1,rou2), max(rou1,rou2), h = h, h.ratio = h.ratio, w = w)
			r = arc.points(theta22, theta21, rou2)
			d = rbind(d, revMat(r))
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
		} else {
	        if(degreeDiff(theta1, theta21) > degreeDiff(theta1, theta22)) {
	        	d1 = getQuadraticPoints(theta1, theta21, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
	        	d2 = getQuadraticPoints(theta1, theta22, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
	        	d1x = getQuadraticPoints(theta1, theta21, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
	        	d2x = getQuadraticPoints(theta1, theta22, rou1, rou2 - arr.length, h = h2, h.ratio = h.ratio, w = w2)
	        	dcenter = getQuadraticPoints(theta1, (theta21 + theta22)/2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
	        } else {
		        d1 = getQuadraticPoints(theta1, theta21, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
		        d2 = getQuadraticPoints(theta1, theta22, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
	        	d1x = getQuadraticPoints(theta1, theta21, rou1, rou2 - arr.length, h = h2, h.ratio = h.ratio, w = w2)
		        d2x = getQuadraticPoints(theta1, theta22, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
	        	dcenter = getQuadraticPoints(theta1, (theta21 + theta22)/2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
		    }
		   	r2 = arc.points(theta22, theta21, rou2)
			if(arr.type == "big.arrow" && directional == 1) {
				# if(nrow(r2) %% 2 == 1) {
				# 	r2 = r2[ceiling(nrow(r2)/2), , drop = FALSE]
				# } else {
				# 	r2 = colMeans(r2[c(nrow(r2)/2, nrow(r2)/2+1), , drop = FALSE])
				# }
				r2 = arc.midpoint(theta22, theta21, rou2)
				d = rbind(d1x, revMat(r2))
				d = rbind(d, revMat(d2x))
			} else {
				d = rbind(d1, revMat(r2))
				d = rbind(d, revMat(d2))
			}

			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
			if(nrow(dcenter) > 1 & arr.type != "big.arrow") {
		        nr = nrow(dcenter)
				if(directional == 1) {
					lines(dcenter[-nr, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == -1) {
					lines(dcenter[-1, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == 2) {
					lines(dcenter[-c(1, nr), , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				}
				if(directional %in% c(1, 2)) {  # point1 to point2
		        	alpha = line_degree(dcenter[nr-1, 1], dcenter[nr-1, 2], dcenter[nr, 1], dcenter[nr, 2])
		        	oljoin = par("ljoin")
	        		par(ljoin = "mitre")
		        	Arrowhead(dcenter[nr, 1], dcenter[nr, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
		        	par(ljoin = oljoin)
		        }
		        if(directional %in% c(-1,2)) {  # point2 to point1
		        	alpha = line_degree(dcenter[2, 1], dcenter[2, 2], dcenter[1, 1], dcenter[1,2])
		        	oljoin = par("ljoin")
		        	par(ljoin = "mitre")
		        	Arrowhead(dcenter[1, 1], dcenter[1, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
		        	par(ljoin = oljoin)
		        }
		    }
		}

	} else if(length(point2) == 1) {
		theta2 = circlize(point2, 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
		theta11 = circlize(point1[1], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta12 = circlize(point1[2], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]

        if(degreeDiff2(theta2, theta11) <= degreeDiff2(theta12, theta11) &
           degreeDiff2(theta12, theta2) <= degreeDiff2(theta12, theta11)) {
			d = getQuadraticPoints(theta12, theta11, max(rou1,rou2), max(rou1,rou2), h = h, h.ratio = h.ratio, w = w)
			r = arc.points(theta12, theta11, rou1)
			d = rbind(d, revMat(r))
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
		} else {
	        if(degreeDiff(theta2, theta11) > degreeDiff(theta2, theta12)) {
		        d1 = getQuadraticPoints(theta11, theta2, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
		        d2 = getQuadraticPoints(theta12, theta2, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
	        	d1x = getQuadraticPoints(theta11, theta2, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
		        d2x = getQuadraticPoints(theta12, theta2, rou1 - arr.length, rou2, h = h2, h.ratio = h.ratio, w = w2)
	        	dcenter = getQuadraticPoints((theta11 + theta12)/2, theta2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
		    } else {
		    	d1 = getQuadraticPoints(theta11, theta2, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
		        d2 = getQuadraticPoints(theta12, theta2, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
	        	d1x = getQuadraticPoints(theta11, theta2, rou1 - arr.length, rou2, h = h2, h.ratio = h.ratio, w = w2)
		        d2x = getQuadraticPoints(theta12, theta2, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
	        	dcenter = getQuadraticPoints((theta11 + theta12)/2, theta2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
		    }
			r1 = arc.points(theta12, theta11, rou1)
			if(arr.type == "big.arrow" && directional == -1) {
				# if(nrow(r1) %% 2 == 1) {
				# 	r1 = r1[ceiling(nrow(r1)/2), , drop = FALSE]
				# } else {
				# 	r1 = colMeans(r1[c(nrow(r1)/2, nrow(r1)/2+1), , drop = FALSE])
				# }
				r1 = arc.midpoint(theta12, theta11, rou1)
				d = rbind(revMat(d1x), revMat(r1))
				d = rbind(d, d2x)
			} else {
				d = rbind(revMat(d1), revMat(r1))
				d = rbind(d, d2)
			}
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
			if(nrow(dcenter) > 1 && arr.type != "big.arrow") {
				nr = nrow(dcenter)
				if(directional == 1) {
					lines(dcenter[-nr, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == -1) {
					lines(dcenter[-1, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == 2) {
					lines(dcenter[-c(1, nr), , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				}
		        if(directional %in% c(1,2)) {  # point1 to point2
		        	alpha = line_degree(dcenter[nr-1, 1], dcenter[nr-1, 2], dcenter[nr, 1], dcenter[nr, 2])
		        	oljoin = par("ljoin")
	        		par(ljoin = "mitre")
	        		Arrowhead(dcenter[nr, 1], dcenter[nr, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
	        		par(ljoin = oljoin)
		        }
		        if(directional %in% c(-1,2)) {  # point2 to point1
		        	alpha = line_degree(dcenter[2, 1], dcenter[2, 2], dcenter[1, 1], dcenter[1,2])
		        	oljoin = par("ljoin")
		        	par(ljoin = "mitre")
		        	Arrowhead(dcenter[1, 1], dcenter[1, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
		        	par(ljoin = oljoin)
		        }
		    }
		}

	} else {

		theta11 = circlize(point1[1], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
        theta12 = circlize(point1[2], 0, sector.index = sector.index1, track.index = 0)[1, "theta"]
		theta21 = circlize(point2[1], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]
        theta22 = circlize(point2[2], 0, sector.index = sector.index2, track.index = 0)[1, "theta"]

		if(degreeDiff2(theta12, theta21) <= degreeDiff2(theta22, theta21) + degreeDiff2(theta12, theta11) &
		   degreeDiff2(theta12, theta21) >= degreeDiff2(theta22, theta11) && abs(rou1 - rou2) < 1e-5) {
			d = getQuadraticPoints(theta12, theta21, max(rou1,rou2), max(rou1,rou2), h = h, h.ratio = h.ratio, w = w)
			r = arc.points(theta12, theta21, max(rou1,rou2))
			d = rbind(d, revMat(r))
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
		} else if(degreeDiff2(theta22, theta11) <= degreeDiff2(theta12, theta11) + degreeDiff2(theta22, theta21) &
			      degreeDiff2(theta22, theta11) >= degreeDiff2(theta12, theta21) && abs(rou1 - rou2) < 1e-5) {
			d = getQuadraticPoints(theta22, theta11, max(rou1,rou2), max(rou1,rou2), h = h, h.ratio = h.ratio, w = w)
			r = arc.points(theta22, theta11, max(rou1,rou2))
			d = rbind(d, revMat(r))
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
		} else {
			if(degreeDiff(theta11, theta22) > degreeDiff(theta12, theta21)) {
		        if(inverse) {
			        d1 = getQuadraticPoints(theta11, theta21, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
			        d2 = getQuadraticPoints(theta12, theta22, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
			    } else {
			    	d1 = getQuadraticPoints(theta11, theta22, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
		       		d2 = getQuadraticPoints(theta12, theta21, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
			    }
		        if(directional == 1) {
		        	if(inverse) {
		        		d1x = getQuadraticPoints(theta11, theta21, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
				        d2x = getQuadraticPoints(theta12, theta22, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
		        	} else {
			        	d1x = getQuadraticPoints(theta11, theta22, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
				        d2x = getQuadraticPoints(theta12, theta21, rou1, rou2 - arr.length, h = h2, h.ratio = h.ratio, w = w2)
				    }
			    } else if(directional == -1) {
			    	if(inverse) {
			    		d1x = getQuadraticPoints(theta11, theta21, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
		        		d2x = getQuadraticPoints(theta12, theta22, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
			    	} else {
				    	d1x = getQuadraticPoints(theta11, theta22, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
			        	d2x = getQuadraticPoints(theta12, theta21, rou1 - arr.length, rou2, h = h2, h.ratio = h.ratio, w = w2)
			        }
			    }
	        	dcenter = getQuadraticPoints((theta11 + theta12)/2, (theta21 + theta22)/2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
		    } else {
		    	if(inverse) {
		    		d1 = getQuadraticPoints(theta11, theta21, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
		        	d2 = getQuadraticPoints(theta12, theta22, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
		    	} else {
			    	d1 = getQuadraticPoints(theta11, theta22, rou1, rou2, h = h2, h.ratio = h.ratio, w = w2)
			        d2 = getQuadraticPoints(theta12, theta21, rou1, rou2, h = h, h.ratio = h.ratio, w = w)
			    }
		        if(directional == 1) {
		        	if(inverse) {
			        	d1x = getQuadraticPoints(theta11, theta21, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
				        d2x = getQuadraticPoints(theta12, theta22, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
				    } else {
				    	d1x = getQuadraticPoints(theta11, theta22, rou1, rou2 - arr.length, h = h2, h.ratio = h.ratio, w = w2)
				        d2x = getQuadraticPoints(theta12, theta21, rou1, rou2 - arr.length, h = h, h.ratio = h.ratio, w = w)
				    }
	        	} else if(directional == -1) {
	        		if(inverse) {
			        	d1x = getQuadraticPoints(theta11, theta21, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
				        d2x = getQuadraticPoints(theta12, theta22, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
				    } else {
				    	d1x = getQuadraticPoints(theta11, theta22, rou1 - arr.length, rou2, h = h2, h.ratio = h.ratio, w = w2)
				        d2x = getQuadraticPoints(theta12, theta21, rou1 - arr.length, rou2, h = h, h.ratio = h.ratio, w = w)
				    }
			    }
	        	dcenter = getQuadraticPoints((theta11 + theta12)/2, (theta21 + theta22)/2, rou1, rou2, h = (h+h2)/2, h.ratio = h.ratio, w = (w+w2)/2)
		    }
			r2 = arc.points(theta22, theta21, rou2)
			r1 = arc.points(theta12, theta11, rou1)

			if(inverse) {
				r2 = revMat(r2)
			}

			if(arr.type == "big.arrow") {
				if(directional == 1) {
					# if(nrow(r2) %% 2 == 1) {
					# 	r2 = r2[ceiling(nrow(r2)/2), , drop = FALSE]
					# } else {
					# 	r2 = colMeans(r2[c(nrow(r2)/2, nrow(r2)/2+1), , drop = FALSE])
					# }
					r2 = arc.midpoint(theta22, theta21, rou2)
					d = rbind(d1x, r2)
			        d = rbind(d, revMat(d2x))
			        d = rbind(d, r1)
			    } else if(directional == -1) {
			  #   	if(nrow(r1) %% 2 == 1) {
					# 	r1 = r1[ceiling(nrow(r1)/2), , drop = FALSE]
					# } else {
					# 	r1 = colMeans(r1[c(nrow(r1)/2, nrow(r1)/2+1), , drop = FALSE])
					# }
					r1 = arc.midpoint(theta12, theta11, rou1)
			    	d = rbind(d1x, r2)
			        d = rbind(d, revMat(d2x))
			        d = rbind(d, r1)
			    } else if(directional == 2) {
			    	r1 = arc.midpoint(theta12, theta11, rou1)
			    	r2 = arc.midpoint(theta22, theta21, rou2)
					d = rbind(d1x, r2)
			        d = rbind(d, revMat(d2x))
			        d = rbind(d, r1)
			    }
			} else {
		        d = rbind(d1, r2)
		        d = rbind(d, revMat(d2))
		        d = rbind(d, r1)
		    }
			polygon(d, col = col, lty = lty, lwd = lwd, border = border)
			if(nrow(dcenter) > 1 && arr.type != "big.arrow") {
				nr = nrow(dcenter)
				if(directional == 1) {
					lines(dcenter[-nr, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == -1) {
					lines(dcenter[-1, , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				} else if(directional == 2) {
					lines(dcenter[-c(1, nr), , drop = FALSE], col = arr.col, lwd = arr.lwd, lty = arr.lty, lend = "butt")
				}

		        if(directional %in% c(1,2)) {  # point1 to point2
		        	alpha = line_degree(dcenter[nr-1, 1], dcenter[nr-1, 2], dcenter[nr, 1], dcenter[nr, 2])
		        	oljoin = par("ljoin")
	        		par(ljoin = "mitre")
		        	Arrowhead(dcenter[nr, 1], dcenter[nr, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1.5, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
		        	par(ljoin = oljoin)
		        }
		        if(directional %in% c(-1,2)) {  # point2 to point1
		        	alpha = line_degree(dcenter[2, 1], dcenter[2, 2], dcenter[1, 1], dcenter[1,2])
		        	oljoin = par("ljoin")
		        	par(ljoin = "mitre")
		        	Arrowhead(dcenter[1, 1], dcenter[1, 2], alpha, arr.length = arr.length, arr.width = arr.width,
		        		arr.adj = 1, arr.lwd = 0.1, arr.type = arr.type, arr.col = arr.col, lcol = arr.col)
		        	par(ljoin = oljoin)
		        }
		    }
		}
    }

    return(invisible(NULL))
}

# points from theta1 to theta2, reverse clockwise
arc.points = function(theta1, theta2, rou) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	theta_diff = (theta2 - theta1) %% 360
	l = as.radian(theta_diff)*rou
	ncut = l/ (2*pi/circos.par("unit.circle.segments"))
	ncut = floor(ncut)
	ncut = ifelse(ncut < 2, 2, ncut)

	x = rou * cos(as.radian(theta1 + seq_len(ncut-1)*theta_diff/(ncut-1)))
	y = rou * sin(as.radian(theta1 + seq_len(ncut-1)*theta_diff/(ncut-1)))
	d = cbind(x, y)
	#linesWithArrows(d)
	return(d)
}

arc.midpoint = function(theta1, theta2, rou) {
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360
	if(theta2 < theta1) theta2 = theta2 + 360
	theta = (theta1 + theta2)/2
	rbind(c(rou*cos(as.radian(theta)), rou*sin(as.radian(theta))))
}

# points from theta1 to theta2
# first calcualte bezier curve of which two end points are located at (-d, 0), (d, 0)
# and the summit is located at (0, 2h)
getQuadraticPoints = function(theta1, theta2, rou1, rou2, h = NULL, h.ratio = 0.5, w = 1) {

	# enforce theta1 is always less than theta 2 (reverse-clockwise)
	theta1 = theta1 %% 360
	theta2 = theta2 %% 360

	if(abs(abs(theta2 - theta1) - 180) < 1e-3) {
		theta1 = theta1 + 1e-3
	}

    if (abs(theta2 - theta1) > 180) {
        theta_mid = (theta2 + theta1)/2 - 180
    } else {
        theta_mid = (theta2 + theta1)/2
    }

    theta_mid  = theta_mid %% 360


	rou_min = min(rou1, rou2)
	x1 = rou_min*cos(as.radian(theta1))
	y1 = rou_min*sin(as.radian(theta1))

	x2 = rou_min*cos(as.radian(theta2))
	y2 = rou_min*sin(as.radian(theta2))

	# determin h
	beta = (theta1 - theta2) %% 360
	if(beta > 180) beta = 360 - beta
	h_auto = rou_min*(1-h.ratio*cos(as.radian(beta/2)))

	if(is.null(h)) {
		h = h_auto
	}
	if(length(h) == 0) {
		h = h_auto
	}
	if(h > rou_min) {
		h = h_auto
	}

	h2 = h - rou_min*(1-cos(as.radian(beta/2)))

	if(w < 0) h2 = -h2

	dis = 1/2 * sqrt((x1 - x2)^2 + (y1 - y2)^2)
	p0 = c(-dis, 0)
	p2 = c(dis, 0)
	if(w == 0) {
		p1 = c(0, 0)
	} else {
		p1 = c(0, (1+w)/w*h2)
	}

	d = quadratic.bezier(p0, p1, p2, w = w)

	# shit, I don't know why!
	col = "red"
	alpha = - 90 + (360 - (theta1 + theta2)/2 %% 360)
	if((theta_mid > 270 || theta_mid < 90) &&
	   ( (theta1 - 180)*(theta2 - 180) < 0 )) {
		alpha = alpha + 180
		col = "green"
	}

	# rotate coordinate
	mat = matrix(c(cos(as.radian(alpha)), sin(as.radian(alpha)), -sin(as.radian(alpha)), cos(as.radian(alpha))), nrow = 2)
	d = d %*% mat

	x0 = (x1+x2)/2
	y0 = (y1+y2)/2

	d[, 1] = d[, 1] + x0
	d[, 2] = d[, 2] + y0

	if((theta1 - theta2) %% 360 < 180) {
		d = revMat(d)
	}

	#points(rou_min*cos(as.radian(theta1)), rou_min*sin(as.radian(theta1)), pch = 16, col = "blue")

	if(rou1 > rou2) {
		d = rbind(c(rou1*cos(as.radian(theta1)), rou1*sin(as.radian(theta1))), d)
	} else if(rou1 < rou2) {
		d = rbind(d, c(rou2*cos(as.radian(theta2)), rou2*sin(as.radian(theta2))))
	}
	#linesWithArrows(d, col = col)
	return(d)
}

# 'Rational bezier curve', from wikipedia
# w: w1
# if I know how to calculate the length of bezier curve, then I can choose a betten n
quadratic.bezier = function(p0, p1, p2, w = 1) {

	ncut = quadratic.bezier.length(p0, p1, p2, w = w)/ (2*pi/circos.par("unit.circle.segments"))
	ncut = floor(ncut)
	ncut = ifelse(ncut < 3, 3, ncut)
	if(ncut %% 2 == 0) ncut = ncut + 1  # odd number

	t = seq(0, 1, length.out = ncut)
	x = ((1-t)^2 * p0[1] + 2*t*(1-t)*p1[1]*w + t^2*p2[1]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	y = ((1-t)^2 * p0[2] + 2*t*(1-t)*p1[2]*w + t^2*p2[2]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	return(cbind(x, y))

}

quadratic.bezier.length = function(p0, p1, p2, w = 1) {
	n = 50
	t = seq(0, 1, length.out = n)
	x = ((1-t)^2 * p0[1] + 2*t*(1-t)*p1[1]*w + t^2*p2[1]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	y = ((1-t)^2 * p0[2] + 2*t*(1-t)*p1[2]*w + t^2*p2[2]) / ((1-t)^2 + 2*t*(1-t)*w + t^2)
	sum(sqrt((x[2:n] - x[1:(n-1)])^2 + (y[2:n] - y[1:(n-1)])^2))

}

# quadratic.bezier = function(p0, p1, p2, w = 1) {
# 	ncut = bezier::bezierArcLength(p = rbind(p0, p1, p2), 0, 1)$arc.length/ (2*pi/circos.par("unit.circle.segments"))
# 	ncut = floor(ncut)
# 	ncut = ifelse(ncut < 3, 3, ncut)
# 	if(ncut %% 2 == 0) ncut = ncut + 1  # odd number

# 	t = seq(0, 1, length.out = ncut)
# 	p = rbind(p0, c(p0[1]*0.5, p0[2]), p1, c(p2[1]*0.5, p2[2]), p2)
# 	bezier::bezier(t, p)
# }

are.lines.intersected = function(x1, y1, x2, y2) {
	n1 = length(x1)
	n2 = length(x2)

	for(i in seq_len(n1)) {
		if(i == 1) next

		a_1x = x1[i-1]
		a_1y = y1[i-1]
		a_2x = x1[i]
		a_2y = y1[i]

		k1 = (a_1y - a_2y)/(a_1x - a_2x)
		b1 = a_1y - k1*a_1x

		for(j in seq_len(n2)) {
			if(j == 1) next

			b_1x = x2[j-1]
			b_1y = y2[j-1]
			b_2x = x2[j]
			b_2y = y2[j]


			k2 = (b_1y - b_2y)/(b_1x - b_2x)
			b2 = b_1y - k2*b_1x

			i_x = - (b2 - b1)/(k2 - k1)
			i_y = k1*i_x + b1

			a_minx = min(c(a_1x, a_2x))
			a_maxx = max(c(a_1x, a_2x))
			b_minx = min(c(b_1x, b_2x))
			b_maxx = max(c(b_1x, b_2x))
			if(i_x >= a_minx && i_x <= a_maxx && i_x >= b_minx && i_x <= b_maxx) {
				return(TRUE)
			}
		}
	}
	return(FALSE)
}

revMat = function(mat) {
	return(mat[rev(seq_len(nrow(mat))), , drop = FALSE])
}


linesWithArrows = function(d, sep = 6, col = "red") {
	lines(d, col = col)
	if(nrow(d) > sep) {
		for(i in seq(sep, nrow(d)-sep, by = sep)) {
			arrows(d[i-sep/2, 1], d[i-sep/2, 2], d[i+sep/2, 1], d[i+sep/2, 2], length = 0.05, col = col)
		}
	} else {
		arrows(d[1, 1], d[1, 2], d[2, 1], d[2, 2], length = 0.05, col = col)
	}
}

# used for links
degreeDiff = function (theta1, theta2) {
    theta1 = theta1 %% 360
    theta2 = theta2 %% 360
    if (abs(theta2 - theta1) > 180) {
        return(abs(theta2 - theta1) - 180)
    }
    else {
        return(abs(theta2 - theta1))
    }
}

# used to calculate whether two arcs overlap
# theta1 is reverse-clockwise of theta2
# e.g. degreeDiff2(90, 180) = 90
#  degreeDiff2(180, 90) = 270
degreeDiff2 = function(theta1, theta2) {
	(theta2 - theta1) %% 360
}

line_degree = function(x0, y0, x1, y1) {
	alpha = (atan((y0 - y1)/(x0 - x1))*180/pi) # -90 ~ 90
	if(x0 > x1 && y0 > y1) alpha = (alpha + 180) %% 360
	if(x0 > x1 && y0 < y1) alpha = (alpha + 180) %% 360
	return(alpha)
}


# == title
# Arrange links evenly on each sector
#
# == param
# -df A data frame with two columns. The values should only contain sector names.
# -directional Whether the links are directional.
#
# == details
# This function only deals with single-line links.
#
# == value
# A data frame with four columns of the sectors and the positions of the links.
#
# == example
# sectors = letters[1:20]
# df = data.frame(from = sample(sectors, 40, replace = TRUE),
#                 to   = sample(sectors, 40, replace = TRUE),
#                 stringsAsFactors = FALSE)
# df = unique(df)
# df = df[df$from != df$to, ]
#
# circos.initialize(sectors, xlim = c(0, 1))
# circos.track(ylim = c(0, 1), panel.fun = function(x, y) {
#     circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index)
# })
#
# df2 = arrange_links_evenly(df, directional = 1)
#
# for(i in seq_len(nrow(df2))) {
#     s1 = df$from[i]
#     s2 = df$to[i]
#     circos.link(df2[i, "sector1"], df2[i, "pos1"], 
#                 df2[i, "sector2"], df2[i, "pos2"],
#                 directional = 1)
# }
#
arrange_links_evenly = function(df, directional = 0) {

    if(!is.circos.initialized()) {
        stop_wrap("Circular layout needs to be initialized.")
    }

    df[, 1] = factor(df[, 1], levels = get.all.sector.index())
    df[, 2] = factor(df[, 2], levels = get.all.sector.index())

    if(any(is.na(df[, 1])) || any(is.na(df[, 2]))) {
    	stop_wrap("The two columns in `df` should only contain sector names.")
    }

    df2 = chordDiagram(df, directional = directional + 0, plot = FALSE)

    link_count = table(unlist(df))
    
    for(i in seq_len(nrow(df2))) {
        s1 = df2[i, 1]
        s2 = df2[i, 2]
        
        xlim1 = get.cell.meta.data("xlim", sector.index = s1)
        df2[i, "x1"] = (df2[i, "x1"] - 0.5)/(link_count[s1]) * (xlim1[2] - xlim1[1]) + xlim1[1]
        xlim2 = get.cell.meta.data("xlim", sector.index = s2)
        df2[i, "x2"] = (df2[i, "x2"] - 0.5)/(link_count[s2]) * (xlim2[2] - xlim2[1]) + xlim2[1]
    }
    df2 = df2[, c("rn", "cn", "x1", "x2")]
    colnames(df2) = c("sector1", "sector2", "pos1", "pos2")
    return(df2)
}

