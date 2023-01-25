# UVM Silk Webhosting Changes
## TCORS Enrollment Dashboard

The UVM TCORS [enrollment report](https://tcors.w3.uvm.edu/enrollment/) is hosted using a UVM Silk hosting service which supports numerous programming language. Users should familiarize themselves with [the documentation](https://silk.uvm.edu/manual/). 

In particular, users looking to push (or pull) changes from this location will need to configure SSH. Connections to this directory are secured with NetID username and password (i.e., user: `tcors`, password:), or through ssh authorized keys. 

From the documentation, a connection to this directory takes the form:

	~$ ssh myusername@w3.uvm.edu
	myusername@w3.uvm.edu's password:
	[myusername@silk1 ~]$


The root directory accessed in a web browser (i.e., [https://tcors.w3.uvm.edu](https://tcors.w3.uvm.edu)) is:

`cd ~/www-root`

The enrollment dashboard is:

`~/www-root/enrollment`

### Pushing changes

I recommend using `rsync` to synchronize files in your local directory with the remote. To update a file called `myfile.txt` in a directory called `mydirectory`, open a terminal (macOS and Linux users; Windows users should use PowerShell) from that directory and execute the following:

	~$ tony@tonyMacBook mydirectory % rsync myfile.txt ssh tcors@w3.uvm.edu:www-root/enrollment/
	tcors@w3.uvm.edu's password: tcors_password
	sending incremental file list
	./	
	myfile.txt
	
	sent 1,220 bytes  received 57 bytes  364.86 bytes/sec
	total size is 1,049,600  speedup is 821.93
	

This will add **or replace** `myfile.txt` on the remote directory with `myfile.txt` in your local directory. 