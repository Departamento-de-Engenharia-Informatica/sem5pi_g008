import { ComponentFixture, TestBed } from '@angular/core/testing';
import { AddAllergyComponent } from './add-allergy.component';
import { AllergyService } from '../../../services/AllergyService/allergyService';
import { Router } from '@angular/router';
import { of, throwError } from 'rxjs';
import { HomeButtonComponent } from '../../Shared/home-button/home-button.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AddAllergyComponent', () => {
  let component: AddAllergyComponent;
  let fixture: ComponentFixture<AddAllergyComponent>;
  let mockAllergyService: jasmine.SpyObj<AllergyService>;
  let mockRouter: jasmine.SpyObj<Router>;

  beforeEach(() => {
    mockAllergyService = jasmine.createSpyObj('AllergyService', ['addAllergy']);
    mockRouter = jasmine.createSpyObj('Router', ['navigate']);

    TestBed.configureTestingModule({
      declarations: [AddAllergyComponent, HomeButtonComponent],
      imports: [HttpClientTestingModule],
      providers: [
        { provide: AllergyService, useValue: mockAllergyService },
        { provide: Router, useValue: mockRouter },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(AddAllergyComponent);
    component = fixture.componentInstance;

    component.allergyDTO = {
      allergyCode: '',
      allergyDesignation: '',
      allergyDescription: '',
      allergyEffects: [],
    };
    component.newEffect = '';
  });

  it('should create the component', () => {
    expect(component).toBeTruthy();
  });

  it('should add an effect to the allergyEffects list', () => {
    component.newEffect = 'Sneezing';
    component.addEffect();
    expect(component.allergyDTO.allergyEffects).toEqual(['Sneezing']);
    expect(component.newEffect).toBe('');
  });

  it('should not add an empty or whitespace effect', () => {
    component.newEffect = '   ';
    component.addEffect();
    expect(component.allergyDTO.allergyEffects.length).toBe(0);
  });

  it('should remove an effect from the allergyEffects list', () => {
    component.allergyDTO.allergyEffects = ['Sneezing', 'Coughing'];
    component.removeEffect(0);
    expect(component.allergyDTO.allergyEffects).toEqual(['Coughing']);
  });

  it('should use the provided description if it is not empty', () => {
    mockAllergyService.addAllergy.and.returnValue(of('Allergy added successfully'));
    component.allergyDTO.allergyDescription = 'Test Description';
    component.addAllergy();
    expect(component.allergyDTO.allergyDescription).toBe('Test Description');
  });

  it('should call the service to add an allergy and navigate on success', () => {
    mockAllergyService.addAllergy.and.returnValue(of('Allergy added successfully'));
    component.addAllergy();
    expect(mockAllergyService.addAllergy).toHaveBeenCalledWith(component.allergyDTO);
    expect(mockRouter.navigate).toHaveBeenCalledWith(['/admin/allergyManagement']);
  });

  it('should show an alert on service error', () => {
    spyOn(window, 'alert');
    mockAllergyService.addAllergy.and.returnValue(throwError('Service error'));
    component.addAllergy();
    expect(window.alert).toHaveBeenCalledWith('Service error');
  });
});
