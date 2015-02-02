# Core-transient
Data and code for NSF funded research on core vs transient species

## How to generate and use an ssh key on a Windows machine
This takes you through the steps necessary to generate an ssh key to work with the core-transient/data submodule. Most of this information is from this [tutorial] https://help.github.com/articles/generating-ssh-keys

1. Open **git bash** 
2. You may as well delete your old core-transient directory now, because we're going to be replacing it (make sure you don't need to push anything prior to!).
```
rm -rf ~/core-transient
```
3. Generate a new ssh key by typing:
```
ssh-keygen -t rsa -C "your_email@example.com"
```
4. You'll be asked to enter your ssh directory and a passphrase. Just leave these blank and hit enter for each select ion. The location of your ssh key and the key fingerprint are printed. The location should be in your home directory.
5. Type `ls .ssh` to be shown the ssh files. _Note: You can type_ `pwd` _to print the working directory location.
7. Navigate to the location of the ssh key in **Windows Explorer** (should be in the home folder of your user name).
8. Open the ssh file, *id_rsa.pub* (There will be two id_rsa files, if you cannot see the extension, it is the one listed as a Microsoft Publisher Document), in **notepad**.
9. Copy the *entire* contents of the file (Ctrl+A, Ctrl+C)
10. In your **web browser**, navigate to your GitHub account online.
  * Click the settings button (upper right-hand corner of your screen, looks like a bicycle sprocket).
  * Click the SSH keys menu option (under the personal settings).
  * Click the "Add SSH key".
  * Provide a title for the key (e.g., "Allens laptop").
  * Paste the **entire** contents of your clipboard in the "key" field and click the "Add key" button.
11. Navigate to the online core-transient [folder] https://github.com/hurlbertlab/core-transient
12. In the lower right you will see a 
11. Navigate back to your **git bash** window. 
12. Test out whether you've successfully connected the key by typing (don't worry if there's a warning):
```
ssh -T git@github.com
```
13. Clone the core-transient folder by typing:
```
git clone git@github.com:hurlbertlab/core-transient.git
```
14. Type `cd core-transient` to navigate to the core-transient folder.
15. Add the submodule folder by typing:
```
git submodule init
git submodule update
```
14. Explore the data folder a bit to find out if you were successful! 

## Priority rankings for obtaining and/or formatting data, on a scale of 0-5, are currently based on summing:

### Taxa (based on current representation): 
  0 birds
  1 Benthos, Fish, Plants
  2 Arthropod, Invertevrate, Plankton, Mammal
  
### System (based on current representation):
  0 marine
  1 terrestrial or aquatic

### Number of sites:
  0 less than 10 or unknown
  1 >= 10

### Number of time intervals:
  0 less than 15 or unknown
  1 greater than 15
