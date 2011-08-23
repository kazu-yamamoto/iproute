# Haskell library for IP addresses and IP routing table.

This library can handle both IPv4 and IPv6 addresses.

The IP routing table is a finite map (or an associative array) based
on the longest match.

The algorithm of the IP routing table is described in
"Radish -- A Simple Routing Table Structure for CIDR"
and its PDF is found on the following page:

	http://www.mew.org/~kazu/proj/iproute/en/
