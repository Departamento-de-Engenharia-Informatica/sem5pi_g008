import {Injectable} from '@angular/core';
import json from "../../appsettings.json"
import {HttpClient, HttpErrorResponse} from '@angular/common/http';
import {catchError, Observable, throwError} from 'rxjs';
import {MedicalConditionMapper} from '../../DTOs/mappers/medicalConditionMapper';
import {MedicalConditionDTO} from "../../DTOs/GenericDTOs/medicalConditionDTO";

@Injectable ({
  providedIn: 'root',
})

export class MedicalConditionService {

  private apiUrl = json.backendApi["2"].url + '/medicalConditions';

  constructor(private http: HttpClient) {
  }

  addMedicalCondition(medicalConditionDTO: MedicalConditionDTO): Observable<string> {

    const medicalCondition = MedicalConditionMapper.domainToBackendDto(medicalConditionDTO);
    const medicalConditionBackendDTO = MedicalConditionMapper.domainToBackendDto(medicalCondition);

    return this.http.post<string>(this.apiUrl, medicalConditionBackendDTO, {
      withCredentials: true,
  }).pipe(

      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status >= 805 && error.status <= 809) {
          errorMessage = error.error.message;
        }

        if(error.status === 500) {
          errorMessage = error.error.message;
        }

        return throwError(errorMessage);
      })
    );
  }

  getAllMedicalConditions(): Observable<any> {
    return this.http.get<any>(this.apiUrl, {
      withCredentials: true,
    }).pipe(
      catchError((error: HttpErrorResponse) => {
        let errorMessage = 'An unknown error occurred.';

        if(error.status >= 805 && error.status <= 809) {
          errorMessage = error.error.message;
        }

        if(error.status === 500) {
          errorMessage = error.error.message;
        }

        return throwError(errorMessage);
      })
    );
  }
}


