# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant::Config.run do |config|
  config.vm.customize ["modifyvm", :id, "--memory", 768]

  config.vm.box = "precise64-popcorn-1"
  config.vm.box_url = "http://download.logwithpopcorn.com/vagrant/precise64-popcorn-1.box"

  config.vm.host_name = "popcorn.local"

  config.vm.network :hostonly, "10.10.10.99"
end
