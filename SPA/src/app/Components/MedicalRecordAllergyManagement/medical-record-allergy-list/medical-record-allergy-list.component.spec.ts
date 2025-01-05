import {MedicalRecordAllergyListComponent} from './medical-record-allergy-list.component';
import {ComponentFixture, TestBed} from '@angular/core/testing';
import {MedicalRecordAllergyService} from '../../../services/medicalRecordAllergyService/medical-record-allergy.service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import {of, throwError} from 'rxjs';
import {DisplayMedicalRecordAllergyDTO} from '../../../DTOs/displayDTOs/displayMedicalRecordAllergyDTO';

describe('MedicalRecordAllergyComponent', () => {

  let component: MedicalRecordAllergyListComponent;
  let fixture: ComponentFixture<MedicalRecordAllergyListComponent>;
  let mockMedicalRecordAllergyService: jasmine.SpyObj<MedicalRecordAllergyService>;

  const mockAllergies: DisplayMedicalRecordAllergyDTO[] = [
    { domainId: '1', allergy: 'Peanuts', medicalRecordId: '123', doctor: 'Dr. Smith', comment: 'Test Comment' },
    { domainId: '2', allergy: 'Dust', medicalRecordId: '123', doctor: 'Dr. Smith', comment: 'Test Comment' },
  ];

  beforeEach(() => {
    mockMedicalRecordAllergyService = jasmine.createSpyObj(
      'MedicalRecordAllergyService', [
      'listAllergiesInMedicalRecord',
    ]);

    TestBed.configureTestingModule({
      declarations: [MedicalRecordAllergyListComponent],
      imports: [HttpClientTestingModule],
      providers: [
        { provide: MedicalRecordAllergyService, useValue: mockMedicalRecordAllergyService },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(MedicalRecordAllergyListComponent);
    component = fixture.componentInstance;
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should fetch and display allergies on init', () => {
    mockMedicalRecordAllergyService.listAllergiesInMedicalRecord.and.returnValue(of(mockAllergies));

    component.medicalRecordId = '123';

    component.ngOnInit();

    expect(mockMedicalRecordAllergyService.listAllergiesInMedicalRecord).toHaveBeenCalledWith('123');
    expect(component.medicalRecordAllergies).toEqual(mockAllergies);
    expect(component.filteredAllergies).toEqual(mockAllergies);
    expect(component.errorMessage).toBe('');
  });

  it('should handle error while fetching allergies', () => {
    mockMedicalRecordAllergyService.listAllergiesInMedicalRecord.and.returnValue(throwError('Failed to fetch'));

    component.medicalRecordId = '123';

    component.ngOnInit();

    expect(mockMedicalRecordAllergyService.listAllergiesInMedicalRecord).toHaveBeenCalledWith('123');
    expect(component.medicalRecordAllergies).toEqual([]);
    expect(component.filteredAllergies).toEqual([]);
    expect(component.errorMessage).toBe('Failed to fetch');
  });

  it('should filter allergies based on search query', () => {
    component.medicalRecordAllergies = mockAllergies;
    component.searchQuery = 'peanuts';

    component.searchAllergy();

    expect(component.filteredAllergies).toEqual([mockAllergies[0]]);
    expect(component.errorMessage).toBe('');
  });

  it('should display an error message if no allergies match the search query', () => {
    component.medicalRecordAllergies = mockAllergies;
    component.searchQuery = 'nonexistent';

    component.searchAllergy();

    expect(component.filteredAllergies).toEqual([]);
    expect(component.errorMessage).toBe('No allergies found for "nonexistent".');
  });

  it('should reset filtered allergies if search query is empty', () => {
    component.medicalRecordAllergies = mockAllergies;
    component.searchQuery = '';

    component.searchAllergy();

    expect(component.filteredAllergies).toEqual(mockAllergies);
    expect(component.errorMessage).toBe('');
  });

});
