import {ComponentFixture, fakeAsync, TestBed, tick} from '@angular/core/testing';
import { EditMedicalConditionComponent } from './edit-medical-condition.component';
import { MedicalConditionService } from '../../../services/MedicalConditionService/medicalConditionService';
import { Router } from '@angular/router';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { HomeButtonComponent } from '../../Shared/home-button/home-button.component';
import { GoBackButtonComponent } from '../../Shared/go-back-button/go-back-button.component';
import { FormsModule } from '@angular/forms';
import { of, throwError } from 'rxjs';
import {MedicalConditionDTO} from '../../../DTOs/GenericDTOs/medicalConditionDTO';

describe('EditMedicalConditionComponent', () => {
  let component: EditMedicalConditionComponent;
  let fixture: ComponentFixture<EditMedicalConditionComponent>;
  let medicalConditionService: jasmine.SpyObj<MedicalConditionService>;
  let router: jasmine.SpyObj<Router>;

  const mockMedicalConditionDTO: MedicalConditionDTO = {
    domainId: '123',
    code: 'C1',
    designation: 'Condition 1',
    description: 'Updated Description',
    symptomsList: ['Fever', 'Headache']
  };

  beforeEach(() => {
    spyOnProperty(window.history, 'state').and.returnValue({
      medicalCondition: {
        domainId: '1',
        designation: 'Condition 1',
        description: 'Test condition description',
        symptomsList: ['Symptom 1'],
        code: 'C1'
      }
    });

    const medicalConditionServiceSpy = jasmine.createSpyObj('MedicalConditionService', [
      'updateMedicalConditionDescription',
      'updateMedicalConditionSymptoms'
    ]);
    const routerSpy = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [EditMedicalConditionComponent, HomeButtonComponent, GoBackButtonComponent],
      imports: [HttpClientTestingModule, FormsModule],
      providers: [
        { provide: MedicalConditionService, useValue: medicalConditionServiceSpy },
        { provide: Router, useValue: routerSpy }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(EditMedicalConditionComponent);
    component = fixture.componentInstance;
    medicalConditionService = TestBed.inject(MedicalConditionService) as jasmine.SpyObj<MedicalConditionService>;
    router = TestBed.inject(Router) as jasmine.SpyObj<Router>;

    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should update medical condition description', async () => {
    medicalConditionService.updateMedicalConditionDescription.and.returnValue(of(void 0));

    component.updatedMedicalCondition.description = 'Updated Description';

    component.editMedicalCondition();
    expect(medicalConditionService.updateMedicalConditionDescription).toHaveBeenCalledWith(
      '1',
      'Updated Description'
    );
  });

  it('should handle error when updateMedicalConditionDescription fails', () => {
    medicalConditionService.updateMedicalConditionDescription.and.returnValue(throwError(() => new Error('Update failed')));

    component.updatedMedicalCondition.domainId = '123';
    component.updatedMedicalCondition.description = 'Updated Description';

    spyOn(window, 'alert');
    component.editMedicalCondition();

    expect(medicalConditionService.updateMedicalConditionDescription).toHaveBeenCalledWith(
      '123',
      'Updated Description'
    );
    expect(window.alert).toHaveBeenCalledWith('Error updating medical condition description.');
    expect(router.navigate).not.toHaveBeenCalled();
  });

  it('should handle error when updateMedicalConditionSymptoms fails', () => {
    medicalConditionService.updateMedicalConditionSymptoms.and.returnValue(throwError(() => new Error('Update failed')));

    component.updatedMedicalCondition.domainId = '123';
    component.updatedMedicalCondition.symptomsList = ['Fever', 'Headache'];

    spyOn(window, 'alert');
    component.editMedicalCondition();

    expect(medicalConditionService.updateMedicalConditionSymptoms).toHaveBeenCalledWith(
      '123',
      ['Fever', 'Headache']
    );
    expect(window.alert).toHaveBeenCalledWith('Error updating medical condition symptoms.');
    expect(router.navigate).not.toHaveBeenCalled();
  });


  it('should call updateMedicalConditionDescription and updateMedicalConditionSymptoms successfully', () => {

    component.updatedMedicalCondition.domainId = '123';
    component.updatedMedicalCondition.description = 'Updated Description';

    medicalConditionService.updateMedicalConditionDescription.and.returnValue(of(void 0));

    component.updatedMedicalCondition.symptomsList = ['Fever', 'Headache'];

    medicalConditionService.updateMedicalConditionSymptoms.and.returnValue(of(mockMedicalConditionDTO));

    component.editMedicalCondition();

    expect(medicalConditionService.updateMedicalConditionDescription).toHaveBeenCalledWith(
      '123',
      'Updated Description'
    );
    expect(medicalConditionService.updateMedicalConditionSymptoms).toHaveBeenCalledWith(
      '123',
      ['Fever', 'Headache']
    );
    expect(router.navigate).toHaveBeenCalledWith(['/admin/medicalConditionManagement']);
  });

  it('should add a symptom to the symptoms list', () => {
    component.newSymptom = 'New Symptom';
    component.addSymptom();
    expect(component.updatedMedicalCondition.symptomsList).toContain('New Symptom');
    expect(component.newSymptom).toBe('');
  });

  it('should not add an empty symptom', () => {
    component.newSymptom = '   ';
    component.addSymptom();
    expect(component.updatedMedicalCondition.symptomsList).not.toContain('   ');
  });

  it('should remove a symptom from the symptoms list', () => {
    component.updatedMedicalCondition.symptomsList = ['Symptom 1', 'Symptom 2'];
    component.removeSymptom(0);
    expect(component.updatedMedicalCondition.symptomsList).toEqual(['Symptom 2']);
  });

  it('should not call services if there are no changes', () => {
    component.updatedMedicalCondition = { ...component.originalMedicalCondition };

    component.editMedicalCondition();

    expect(medicalConditionService.updateMedicalConditionDescription).not.toHaveBeenCalled();
    expect(medicalConditionService.updateMedicalConditionSymptoms).not.toHaveBeenCalled();
    expect(router.navigate).toHaveBeenCalledWith(['/admin/medicalConditionManagement']);
  });
});
