import { ComponentFixture, TestBed } from '@angular/core/testing';
import { AddMedicalConditionComponent } from './add-medical-condition.component';
import { MedicalConditionService } from '../../../services/MedicalConditionService/medicalConditionService';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { HomeButtonComponent } from '../../Shared/home-button/home-button.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AddMedicalConditionComponent', () => {
  let component: AddMedicalConditionComponent;
  let fixture: ComponentFixture<AddMedicalConditionComponent>;
  let mockMedicalConditionService: jasmine.SpyObj<MedicalConditionService>;
  let mockRouter: jasmine.SpyObj<Router>;

  beforeEach(() => {
    mockMedicalConditionService = jasmine.createSpyObj('MedicalConditionService', ['addMedicalCondition']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [AddMedicalConditionComponent, HomeButtonComponent],
      imports: [HttpClientTestingModule],
      providers: [
        { provide: MedicalConditionService, useValue: mockMedicalConditionService },
        { provide: Router, useValue: mockRouter },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(AddMedicalConditionComponent);
    component = fixture.componentInstance;

    component.medicalConditionDTO = { description: '', code: '', designation: '' };
    component.description = '';
    component.symptomsList = [];
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should add a symptom to the list', () => {
    component.newSymptom = 'Headache';
    component.addSymptom();
    expect(component.symptomsList).toEqual(['Headache']);
    expect(component.newSymptom).toBe('');
  });

  it('should not add an empty symptom', () => {
    component.newSymptom = '   ';
    component.addSymptom();
    expect(component.symptomsList.length).toBe(0);
  });

  it('should remove a symptom from the list', () => {
    component.symptomsList = ['Headache', 'Fever'];
    component.removeSymptom(0);
    expect(component.symptomsList).toEqual(['Fever']);
  });

  it('should set a default description if no description is provided', () => {
    mockMedicalConditionService.addMedicalCondition.and.returnValue(of('Medical condition added successfully'));
    component.description = '';
    component.addMedicalCondition();
    expect(component.medicalConditionDTO.description).toBe('No Description Provided');
  });

  it('should use the provided description if it is not empty', () => {
    mockMedicalConditionService.addMedicalCondition.and.returnValue(of('Medical condition added successfully'));
    component.description = 'Test Description';
    component.addMedicalCondition();
    expect(component.medicalConditionDTO.description).toBe('Test Description');
  });

  it('should call the service to add a medical condition and navigate on success', () => {
    mockMedicalConditionService.addMedicalCondition.and.returnValue(of('Medical condition added successfully'));
    component.addMedicalCondition();
    expect(mockMedicalConditionService.addMedicalCondition).toHaveBeenCalledWith(component.medicalConditionDTO);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/admin/medicalConditionManagement']);
  });

});
