# regex_spec.rb
require 'rspec'

RSpec.describe 'Regular Expression for decimal literal' do
  
  let(:signOpt) {/-?/}
  let(:digit) {/\d+/}
  let(:thousandSep) {/(?:'\d{3})*/}
  let(:size) {/[uU]?[lL]{0,2}/}
  
  let(:regex) {/^#{signOpt}#{digit}#{thousandSep}#{size}$/}

  let(:octal_digit) {  /[0-7]/ }
  let(:hex_digit) {  /[0-9]|[A-F]|[a-f]/ }
  let(:binary_digit) {  /[0-1]/ }
  
  let(:should_pass) { [ "1", "-33'000", "4525235", "10'080", "123'456'789", "1ul", "1u", "1ll", "0"] }
  let(:should_fail) { ["'1'", "1'''3", "afed", "+33", "ul", "lll", "3lll", "3uuull"] }

  describe 'should pass' do
    it 'matches all expected strings' do
      should_pass.each do |str|
        expect(str).to match(regex)
      end
    end
  end

  describe 'should fail' do
    it 'does not match any of the strings' do
      should_fail.each do |str|
        expect(str).not_to match(regex)
      end
    end
  end
end

RSpec::Core::Runner.run([])

# To check the results programmatically
puts "RSpec results: #{RSpec.world.filtered_examples.values.flatten.map(&:execution_result).map(&:status)}"