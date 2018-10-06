#' NOMINATE/VoteView.com color palette
#'
#' The standard color scheme for party affiliation and votes by party affiliation.
#'
#' @name scale_voteview
#'
#Roll call votes by party affiliation.
Party_Member_Vote <- factor( c('Dem: Yea',
                  'Rep: Yea',
                  'Dem: Nay',
                  'Rep: Nay',
                  'Dem: Not Voting',
                  'Rep: Not Voting')
                )

Party_Member_Vote <- factor(Party_Member_Vote,
               levels(Party_Member_Vote)[c(3,6,1,4,2,5)])


#Roll call votes by party affiliation -- shapes.
vote_shapes <- c(24, 24, 25, 25, 21, 21)


#Traditional party colors.
party <- c('Dem', 'Rep')
party_pal <- c('#378dbf',
                  '#d4334c')

#VoteView palette.
voteview_pal <- c('#378dbf',
                  '#d4334c',
                  '#a7d0e4',
                  '#f6b79b',
                  '#e4eef4',
                  '#fae9df')


#scale_colour_manual(
#  values = cols,
#  aesthetics = c("colour", "fill"))


#' @rdname scale_voteview
#' @export
voteview_pal <- voteview_pal

#' @rdname scale_voteview
#' @export
party_pal <- party_pal

#' @rdname scale_voteview
#' @export
scale_color_rollcall <- function (...) {
  ggplot2::scale_color_manual(name = "Party_Member_Vote", values = voteview_pal, ...)
}

#' @rdname scale_voteview
#' @export
scale_fill_rollcall <- function (...) {
  ggplot2::scale_fill_manual(name = "Party_Member_Vote", values = voteview_pal, ...)
}

#' @rdname scale_voteview
#' @export
scale_color_party <- function (...) {
  ggplot2::scale_color_manual(name = "party", values = party_pal, ...)
} #Presently, does not include Independents.

#' @rdname scale_voteview
#' @export
scale_shape_rollcall <- function (...) {
  ggplot2::scale_shape_manual(name = "Party_Member_Vote", values = vote_shapes, ...)
}
