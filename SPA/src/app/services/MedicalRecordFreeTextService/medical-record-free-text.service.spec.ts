import {TestBed} from '@angular/core/testing';
import {MedicalRecordFreeTextService} from './medical-record-free-text.service';

describe('MedicalRecordFreeTextService', () => {
  let service: MedicalRecordFreeTextService;

  beforeEach(() => {
    TestBed.configureTestingModule({});
    service = TestBed.inject(MedicalRecordFreeTextService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
