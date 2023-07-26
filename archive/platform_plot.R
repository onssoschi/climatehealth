output_df_reg <- output_df[output_df$regions == "East", ]
tmean_df_reg <- tmean_df[tmean_df$regions == "East", ]

per <- quantile(tmean_df_reg$temp_mean, c(2.5,10,25,50,75,90,97.5)/100, na.rm = T)

xlab <- expression(paste("Temperature (", degree, "C)"))

ind_a <- output_df_reg$temp <= c(per[i, c("2.5%")])
ind_b <- output_df_reg$temp >= c(per[i, c("2.5%")]) & output_df_reg$temp <= output_df_reg$centre_temp
ind_c <- output_df_reg$temp >= output_df_reg$centre_temp & output_df_reg$temp <= c(per[c("97.5%")])
ind_d <- output_df_reg$temp >= c(per[i, c("97.5%")])

plot(min(output_df_reg$temp):max(output_df_reg$temp),
     type = "n",
     ylim = c(0.5, 2.5),
     xlim = c(min(output_df_reg$temp), max(output_df_reg$temp)),
     yaxt = "n",
     lab = c(6,5,7),
     xlab = xlab,
     ylab = "Relative Risk",
     main = unique(output_df_reg$regions))

axis(2, at=c(0.5, 1, 1.5, 2, 2.5))

lines(output_df_reg$temp[ind_a],
      output_df_reg$rel_risk[ind_a],
      col = c("#000FFF"),
      lwd = 2)

lines(output_df_reg$temp[ind_b],
      output_df_reg$rel_risk[ind_b],
      col = c("#ABAFFF"),
      lwd = 2)

lines(output_df_reg$temp[ind_c],
      output_df_reg$rel_risk[ind_c],
      col = c("#FFA7A7"),
      lwd = 2)

lines(output_df_reg$temp[ind_d],
      output_df_reg$rel_risk[ind_d],
      col = c("#C00000"),
      lwd = 2)

breaks <- c(min(tmean_df_reg$temp_mean, na.rm = T)-1,
            seq(output_df_reg$temp[1],
                output_df_reg$temp[length(output_df_reg$temp)],
                length = 30),
            max(tmean_df_reg$temp_mean,na.rm = T) + 1)

abline(v = output_df_reg$centre_temp, lty = 3)
abline(h = 1, lty = 3)
abline(v = c(per[c("2.5%","97.5%")]), lty = 2)
